module Github = Current_github
module Git = Current_git
open Common
open Current.Syntax

module Github_repository = struct
  type t = {
    owner : string;
    name : string;
    ci : (Github.Api.Ref.t * Github.Api.Commit.t) list Current.t;
    branch : Github.Api.Commit.t Current.t;
    all : (Github.Api.Ref.t * Github.Api.Commit.t) list Current.t;
  }

  let equal v1 v2 = v1.name = v2.name && v1.owner = v2.owner

  let repo_refs ~github repo =
    let refs = Github.Api.refs github repo in
    Current.primitive
      ~info:(Current.component "repository refs")
      (fun () -> refs)
      (Current.return ())

  let github_setup ~branch ~github owner name =
    let ref_filter = function
      | `PR Github.Api.Ref.{ base; _ } when base = branch -> true
      | `Ref ref when ref = "refs/heads/" ^ branch -> true
      | _ -> false
    in
    let open Github in
    let gh = { Github.Repo_id.owner; name } in
    let ci_refs =
      Github.Api.ci_refs ~staleness:(Duration.of_day 90) github gh
    in
    let ci =
      let+ refs = repo_refs ~github gh and+ ci_refs = ci_refs in
      let map = Github.Api.all_refs refs in
      List.filter_map
        (fun commit ->
          Api.Ref_map.filter
            (fun _ commit' -> Api.Commit.(hash commit' = hash commit))
            map
          |> Api.Ref_map.bindings
          |> List.find_map (fun (ref, _) ->
                 if ref_filter ref then Some (ref, commit) else None))
        ci_refs
    in
    let all =
      let+ refs = repo_refs ~github gh in
      Github.Api.all_refs refs |> Github.Api.Ref_map.bindings
    in
    let branch = Github.Api.head_of github gh (`Ref ("refs/heads/" ^ branch)) in
    { owner; name; ci; branch; all }
end

type t =
  ( unit,
    string,
    string,
    Website.Website_description.Pipeline.t )
  Current_web_pipelines.State.pipeline

let gh_url (meta, stage) =
  Uri.of_string
    (Fmt.str "https://ci.mirage.io%s"
       (Website.pipeline_stage_url (`Github meta) stage))

let github_status_of_state meta status =
  let url = gh_url meta in
  match status with
  | Ok _ -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m) -> Github.Api.Status.v ~url `Failure ~description:m

module Friend_PR = struct
  type t = { owner : string; name : string; id : int }

  let pull_request_regex =
    Str.regexp
      "https://github.com/\\([A-Za-z0-9\\-]+\\)/\\([A-Za-z0-9\\-]+\\)/pull/\\([0-9]+\\)"

  let find_friend_prs html =
    let open Soup in
    let html = parse html in
    html
    $$ "a"
    |> to_list
    |> List.filter_map (fun link ->
           let content = attribute "href" link |> Option.value ~default:"" in
           if Str.string_match pull_request_regex content 0 then
             let owner = Str.matched_group 1 content in
             let name = Str.matched_group 2 content in
             let id = Str.matched_group 3 content |> int_of_string in
             Some { owner; name; id }
           else None)

  let find_friend_prs = function
    | `PR Github.Api.Ref.{ bodyHTML; _ } -> find_friend_prs bodyHTML
    | _ -> []

  let resolve friends (repo : Github_repository.t) =
    let+ friends = friends and+ refs = repo.all and+ branch = repo.branch in
    List.find_map
      (fun { owner; name; id } ->
        if repo.owner = owner && repo.name = name then
          List.find_map
            (function
              | `PR (Github.Api.Ref.{ id = id'; _ } as pr), value when id' = id
                ->
                  Some (pr, Github.Api.Commit.id value)
              | _ -> None)
            refs
        else None)
      friends
    |> function
    | None -> (None, Github.Api.Commit.id branch)
    | Some (pr, value) -> (Some (repo.owner ^ "/" ^ repo.name, pr), value)
end

module Run = struct
  module Raw = struct
    type t = {
      mirage_dev : Git.Commit_id.t Current.t option;
      config : Config.t;
      platform : Platform.t;
      mirage_skeleton : Git.Commit_id.t Current.t;
      mirage : Git.Commit_id.t Current.t option;
      repos : Opam_repository.t list Current.t;
      build_mode : Opam_repository.t list Current.t Skeleton.build_mode;
    }

    let perform_test
        {
          mirage_dev;
          config;
          platform;
          mirage_skeleton;
          mirage;
          repos;
          build_mode;
        } =
      let repos =
        match mirage_dev with
        | None -> repos
        | Some mirage_dev ->
            let+ repos = repos and+ mirage_dev = mirage_dev in
            repos @ [ ("mirage-dev", mirage_dev) ]
      in
      Skeleton.all_in_one_test ~platform ~repos ~mirage ~build_mode ~config
        mirage_skeleton
  end

  module Gh = struct
    type t = {
      raw : Raw.t;
      gh_commit : Github.Api.Commit.t Current.t;
      commit_status : bool;
    }

    let perform_test_and_report_status ~metadata
        { raw = { platform; _ } as raw; gh_commit; commit_status } =
      let pipeline = Raw.perform_test raw in
      let open Current_web_pipelines in
      let state = Task.state pipeline in
      let commit_status =
        match commit_status with
        | false -> Current_web_pipelines.Task.current pipeline
        | true ->
            let status =
              let+ state =
                Current_web_pipelines.Task.current pipeline
                |> Current.state ~hidden:true
              and+ metadata = metadata
              and+ stage_state = state in
              github_status_of_state (metadata, stage_state.metadata) state
            in
            Github.Api.Commit.set_status gh_commit
              (Fmt.str "Mirage CI - %a" Platform.pp_platform platform)
              status
      in
      (* OK because commit_status is derived from pipeline *)
      Task.v ~current:commit_status ~state
  end

  module Pipeline_run = struct
    type github_tracked_repositories = {
      mirage : Github_repository.t option;
      mirage_dev : Github_repository.t option;
      mirage_skeleton : Github_repository.t;
      build_mode : (string * Github_repository.t) list Skeleton.build_mode;
    }

    let involved_repositories
        { mirage; mirage_dev; mirage_skeleton; build_mode } =
      (mirage |> Option.map (fun v -> (`Mirage, v)) |> Option.to_list)
      @ (mirage_dev |> Option.map (fun v -> (`Mirage_dev, v)) |> Option.to_list)
      @ [ (`Mirage_skeleton, mirage_skeleton) ]
      @
      match build_mode with
      | Mirage_4 { overlay = Some overlay } ->
          List.map (fun (name, repo) -> (`Overlay name, repo)) overlay
      | _ -> []

    type github = {
      config : Config.t;
      repos : Opam_repository.t list Current.t;
      tracked_repositories : github_tracked_repositories;
      commit_status : bool;
    }

    type local = {
      config : Config.t;
      repos : Opam_repository.t list Current.t;
      mirage : Git.Commit_id.t Current.t;
      mirage_skeleton : Git.Commit_id.t Current.t;
      mirage_dev : Git.Commit_id.t Current.t option;
      build_mode : Opam_repository.t list Current.t Skeleton.build_mode;
    }

    let local { mirage; mirage_dev; mirage_skeleton; build_mode; config; repos }
        =
      (* same pipeline, three sets of metadata. *)
      let pipeline =
        Raw.perform_test
          {
            config;
            repos;
            mirage_dev;
            platform = Common.Platform.platform_host;
            mirage_skeleton;
            mirage = Some mirage;
            build_mode;
          }
      in
      let state =
        let+ stage = Current_web_pipelines.Task.state pipeline in
        [
          {
            Current_web_pipelines.State.stages = [ stage ];
            metadata =
              (match build_mode with
              | Skeleton.Mirage_3 -> `Mirage_3
              | _ -> `Mirage_4);
          };
        ]
      in
      Current_web_pipelines.Task.v
        ~current:(Current_web_pipelines.Task.current pipeline)
        ~state

    module Commit = struct
      type t = Github.Api.Ref.t * Github.Api.Commit.t

      let pp f (ref, _) = Fmt.pf f "Github: %a" Github.Api.Ref.pp ref
      let compare (_, a) (_, b) = Github.Api.Commit.compare a b
    end

    (* Friend PR resolution *)
    let friend_pr ~friends mirage_skeleton =
      let resolved = Friend_PR.resolve friends mirage_skeleton in
      (Current.map fst resolved, Current.map snd resolved)

    let id_of gh_commit = Current.map Github.Api.Commit.id gh_commit

    let build_mode = function
      | Skeleton.Mirage_3 -> `Mirage_3
      | Mirage_4 _ -> `Mirage_4

    let resolve_repo ~friends source_repo repo =
      if Github_repository.equal source_repo repo then
        (Current.return None, fun commit -> id_of commit)
      else
        let friend_pr_info, commit = friend_pr ~friends repo in
        (friend_pr_info, fun _ -> commit)

    let resolve_build_mode ~friends source_repo
        (github_tracked_repositories : github_tracked_repositories) =
      match github_tracked_repositories.build_mode with
      | Skeleton.Mirage_3 -> (Current.return [], Skeleton.Mirage_3)
      | Mirage_4 { overlay = None } ->
          (Current.return [], Mirage_4 { overlay = None })
      | Mirage_4 { overlay = Some v } ->
          let overlays =
            (List.map (fun (name, repo) ->
                 let friends, info = resolve_repo ~friends source_repo repo in
                 (friends, (name, info))))
              v
          in
          let overlays_friends =
            overlays
            |> List.map fst
            |> Current.list_seq
            |> Current.map (List.filter_map Fun.id)
          in
          let overlays_info = List.map snd overlays in
          (overlays_friends, Mirage_4 { overlay = Some overlays_info })

    let resolve_mirage ~friends source_repo
        (github_tracked_repositories : github_tracked_repositories) =
      let repo = github_tracked_repositories.mirage in
      match repo with
      | None -> (Current.return [], None)
      | Some repo ->
          let friend_prs, resolved = resolve_repo ~friends source_repo repo in
          (Current.map Option.to_list friend_prs, Some resolved)

    let resolve_mirage_dev ~friends source_repo
        (github_tracked_repositories : github_tracked_repositories) =
      let repo = github_tracked_repositories.mirage_dev in
      match repo with
      | None -> (Current.return [], None)
      | Some repo ->
          let friend_prs, resolved = resolve_repo ~friends source_repo repo in
          (Current.map Option.to_list friend_prs, Some resolved)

    let resolve_mirage_skeleton ~friends source_repo
        (github_tracked_repositories : github_tracked_repositories) =
      let repo = github_tracked_repositories.mirage_skeleton in
      let friend_prs, resolved = resolve_repo ~friends source_repo repo in
      (Current.map Option.to_list friend_prs, resolved)

    let build_mode_map fn = function
      | Skeleton.Mirage_3 -> Skeleton.Mirage_3
      | Mirage_4 { overlay } -> Mirage_4 { overlay = (Option.map fn) overlay }

    let friend_pr_merge lst = Current.list_seq lst |> Current.map List.flatten

    let github { tracked_repositories; commit_status; repos; config }
        (repo : Github_repository.t) =
      let perform_test ~friends =
        let friend_pr_mirage_dev, mirage_dev =
          resolve_mirage_dev ~friends repo tracked_repositories
        in
        let friend_pr_mirage_skeleton, mirage_skeleton =
          resolve_mirage_skeleton ~friends repo tracked_repositories
        in
        let friend_pr_mirage, mirage =
          resolve_mirage ~friends repo tracked_repositories
        in
        let friend_pr_build_mode, build_mode =
          resolve_build_mode ~friends repo tracked_repositories
        in
        ( friend_pr_merge
            [
              friend_pr_mirage;
              friend_pr_mirage_dev;
              friend_pr_mirage_skeleton;
              friend_pr_build_mode;
            ],
          fun ~metadata ~platform commit ->
            let mirage = Option.map (fun v -> v commit) mirage in
            let mirage_skeleton = mirage_skeleton commit in
            let mirage_dev = Option.map (fun v -> v commit) mirage_dev in
            let build_mode =
              build_mode_map
                (fun v ->
                  v
                  |> List.map (fun (name, fn) ->
                         let+ commit = fn commit in
                         (name, commit))
                  |> Current.list_seq)
                build_mode
            in
            Gh.perform_test_and_report_status ~metadata
              {
                Gh.raw =
                  {
                    platform;
                    mirage_dev;
                    mirage_skeleton;
                    mirage;
                    repos;
                    build_mode;
                    config;
                  };
                gh_commit = commit;
                commit_status;
              } )
      in
      let kind =
        involved_repositories tracked_repositories
        |> List.find_map (fun (name, tracked_repo) ->
               if Github_repository.equal repo tracked_repo then Some name
               else None)
        |> Option.get
      in

      repo.ci
      |> Current_web_pipelines.Task.list_iter ~collapse_key:"pipelines"
           (module Commit)
           (fun commit ->
             let ref = Current.map fst commit in
             let commit = Current.map snd commit in
             let friends = Current.map Friend_PR.find_friend_prs ref in
             let friend_prs, perform_test = perform_test ~friends in

             let metadata =
               let+ friend_prs = friend_prs
               and+ ref = ref
               and+ commit = commit in
               {
                 Website.Website_description.Pipeline.Source.ref;
                 friend_prs;
                 kind;
                 owner = repo.owner;
                 name = repo.name;
                 commit = Github.Api.Commit.id commit |> Git.Commit_id.hash;
                 build_mode = build_mode tracked_repositories.build_mode;
               }
             in

             let pipeline =
               Platform.[ platform_v413_amd64; platform_v413_arm64 ]
               |> List.map (fun platform ->
                      perform_test ~metadata ~platform commit)
               |> Current_web_pipelines.Task.all
             in
             let state =
               let+ stages = Current_web_pipelines.Task.state pipeline
               and+ metadata = metadata in
               { Current_web_pipelines.State.stages; metadata }
             in
             Current_web_pipelines.Task.v
               ~current:(Current_web_pipelines.Task.current pipeline)
               ~state)
  end
end

(*
let pp_url ~(repo : Github.Repo_id.t) f (ref : Github.Api.Ref.t) =
  match ref with
  | `Ref ref ->
      Fmt.pf f "https://github.com/%s/%s/tree/%s" repo.owner repo.name ref
  | `PR { id; _ } ->
      Fmt.pf f "https://github.com/%s/%s/pull/%d" repo.owner repo.name id

let url_of_commit (commit : Github.Api.Commit.t) (ref : Github.Api.Ref.t) =
  let open Github in
  let repo = Api.Commit.repo_id commit in
  (Fmt.str "Github: %a" Api.Ref.pp ref, Fmt.to_to_string (pp_url ~repo) ref)
*)
type enable_commit_status = {
  mirage : bool;
  skeleton : bool;
  dev : bool;
  overlay : bool;
}

type test_options = {
  mirage_4 : enable_commit_status option;
  mirage_3 : enable_commit_status option;
}

let is_enabled t = Option.is_some t.mirage_4 || Option.is_some t.mirage_3

let test_options_cmdliner =
  let open Cmdliner in
  let status =
    [
      ("mirage", `Mirage);
      ("skeleton", `Skeleton);
      ("dev", `Dev);
      ("overlay", `Overlay);
    ]
  in
  let conv_commit_status =
    Arg.(opt ~vopt:(Some []) (some (list (enum status))) None)
  in
  let mirage_4 =
    Arg.value (conv_commit_status (Arg.info [ "test-mirage-4" ]))
  in
  let mirage_3 =
    Arg.value (conv_commit_status (Arg.info [ "test-mirage-3" ]))
  in
  let make mirage_3 mirage_4 =
    let make_commit_status list =
      List.fold_left
        (fun acc -> function
          | `Mirage -> { acc with mirage = true }
          | `Skeleton -> { acc with skeleton = true }
          | `Dev -> { acc with dev = true }
          | `Overlay -> { acc with overlay = true })
        { mirage = false; skeleton = false; dev = false; overlay = false }
        list
    in
    {
      mirage_4 = Option.map make_commit_status mirage_4;
      mirage_3 = Option.map make_commit_status mirage_3;
    }
  in
  Term.(const make $ mirage_3 $ mirage_4)

type context = {
  config : Common.Config.t;
  enable_commit_status : enable_commit_status;
  repos : Opam_repository.t list Current.t;
}

let pipeline ~mirage ~mirage_skeleton ~mirage_dev ~build_mode
    { config; enable_commit_status; repos } =
  let config_with_mirage =
    {
      Run.Pipeline_run.mirage = Some mirage;
      mirage_dev;
      mirage_skeleton;
      build_mode;
    }
  in
  let config_without_mirage =
    { Run.Pipeline_run.mirage = None; mirage_dev; mirage_skeleton; build_mode }
  in

  let pipelines =
    [
      ( mirage_skeleton,
        {
          Run.Pipeline_run.config;
          repos;
          tracked_repositories = config_with_mirage;
          commit_status = enable_commit_status.skeleton;
        } );
      ( mirage,
        {
          Run.Pipeline_run.config;
          repos;
          tracked_repositories = config_with_mirage;
          commit_status = enable_commit_status.mirage;
        } );
    ]
    @ (match mirage_dev with
      | Some mirage_dev ->
          [
            ( mirage_dev,
              {
                Run.Pipeline_run.config;
                repos;
                tracked_repositories = config_without_mirage;
                commit_status = enable_commit_status.dev;
              } );
          ]
      | None -> [])
    @
    match build_mode with
    | Mirage_4 { overlay = None } -> []
    | Mirage_4 { overlay = Some overlays } ->
        List.map
          (fun (_, repo) ->
            ( repo,
              {
                Run.Pipeline_run.config;
                repos;
                tracked_repositories = config_without_mirage;
                commit_status = enable_commit_status.overlay;
              } ))
          overlays
    | Mirage_3 -> []
  in
  List.map
    (fun (repo, config) ->
      let label = repo.Github_repository.owner ^ "/" ^ repo.name in
      Run.Pipeline_run.github config repo
      |> Current_web_pipelines.Task.apply_current
           (Current.collapse ~key:"repo" ~value:label
              ~input:(Current.return ~label ())))
    pipelines
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state List.flatten

type repo = { org : string; name : string; branch : string }

type test_set = {
  enable_commit_status : enable_commit_status;
  mirage : repo;
  mirage_skeleton : repo;
  mirage_dev : repo option;
  build_mode : (string * repo) list Skeleton.build_mode;
}

let tests options =
  let m4 =
    Option.map
      (fun enable_commit_status ->
        {
          enable_commit_status;
          mirage = { org = "mirage"; name = "mirage"; branch = "main" };
          mirage_dev =
            Some { org = "mirage"; name = "mirage-dev"; branch = "master" };
          mirage_skeleton =
            { org = "mirage"; name = "mirage-skeleton"; branch = "mirage-dev" };
          build_mode =
            Mirage_4
              {
                overlay =
                  Some
                    [
                      ( "opam-overlays",
                        {
                          org = "dune-universe";
                          name = "opam-overlays";
                          branch = "master";
                        } );
                      ( "mirage-opam-overlays",
                        {
                          org = "dune-universe";
                          name = "mirage-opam-overlays";
                          branch = "main";
                        } );
                    ];
              };
        })
      options.mirage_4
  in
  let m3 =
    Option.map
      (fun enable_commit_status ->
        {
          enable_commit_status;
          mirage = { org = "mirage"; name = "mirage"; branch = "3" };
          mirage_dev =
            Some { org = "mirage"; name = "mirage-dev"; branch = "3" };
          mirage_skeleton =
            { org = "mirage"; name = "mirage-skeleton"; branch = "master" };
          build_mode = Mirage_3;
        })
      options.mirage_3
  in
  Option.to_list m4 @ Option.to_list m3

(* WE PERFORM TWO SETS OF TESTS
   - mirage skeleton 'master' / mirage '3' / mirage-dev '3'
   - mirage skeleton 'mirage-dev' / mirage 'main' / mirage-dev 'master' *)
let make ~config ~options ~repos github =
  let github_setup { branch; org; name } =
    Github_repository.github_setup ~branch ~github org name
  in
  tests options
  |> List.map
       (fun
         {
           enable_commit_status;
           mirage;
           mirage_dev;
           mirage_skeleton;
           build_mode;
         }
       ->
         let ctx = { config; enable_commit_status; repos } in
         let mirage = github_setup mirage in
         let mirage_skeleton = github_setup mirage_skeleton in
         let mirage_dev = Option.map github_setup mirage_dev in
         let build_mode =
           Run.Pipeline_run.build_mode_map
             (List.map (fun (name, repo) -> (name, github_setup repo)))
             build_mode
         in
         (* todo *)
         pipeline ~mirage ~mirage_skeleton ~mirage_dev ~build_mode ctx)
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state List.flatten
  |> Current_web_pipelines.Task.map_state
       (List.map (fun (t : _ Current_web_pipelines.State.pipeline) ->
            { t with metadata = `Github t.metadata }))

let local ~config ~options ~repos =
  let github_setup { branch; org; name } =
    Github.Api.Anonymous.head_of { owner = org; name }
      (`Ref ("refs/heads/" ^ branch))
  in
  tests options
  |> List.map (fun { mirage; mirage_dev; mirage_skeleton; build_mode; _ } ->
         let mirage = github_setup mirage in
         let mirage_skeleton = github_setup mirage_skeleton in
         let mirage_dev = Option.map github_setup mirage_dev in
         let build_mode =
           Run.Pipeline_run.build_mode_map
             (fun v ->
               v
               |> List.map (fun (name, repo) ->
                      Current.map (fun v -> (name, v)) (github_setup repo))
               |> Current.list_seq)
             build_mode
         in
         (* same pipeline, three sets of metadata. *)
         Run.Pipeline_run.local
           { config; repos; mirage_dev; mirage_skeleton; mirage; build_mode })
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state List.flatten
  |> Current_web_pipelines.Task.map_state
       (List.map (fun (t : _ Current_web_pipelines.State.pipeline) ->
            { t with metadata = `Local t.metadata }))
