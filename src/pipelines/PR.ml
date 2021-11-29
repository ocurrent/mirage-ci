module Github = Current_github
module Git = Current_git
open Common

module Repo = struct
  type t = {
    owner : string;
    name : string;
    ci : (Github.Api.Ref.t * Github.Api.Commit.t) list Current.t;
    branch : Github.Api.Commit.t Current.t;
    all : (Github.Api.Ref.t * Github.Api.Commit.t) list Current.t;
  }

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
    let open Current.Syntax in
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

(* the exposed metadata *)
type metadata_gh = {
  kind : [ `Mirage | `Mirage_dev | `Mirage_skeleton | `Opam_overlays ];
  build_mode : [ `Mirage_4 | `Mirage_3 ];
  commit : string;
  ref : Github.Api.Ref.t;
  owner : string;
  name : string;
  friend_prs : Github.Api.Ref.pr_info list;
}

type pipeline = [ `Local of [ `Mirage_4 | `Mirage_3 ] | `Github of metadata_gh ]
type t = (unit, string, string, pipeline) Current_web_pipelines.State.pipeline

let compare_metadata = Stdlib.compare

(* TODO!!!! *)
let url kind id =
  Uri.of_string (Fmt.str "https://ci.mirage.io/github/%s/prs/%s" kind id)

let github_status_of_state kind id status =
  let url = url kind id in
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

  let resolve friends repo =
    let open Current.Syntax in
    let+ friends = friends
    and+ refs = repo.Repo.all
    and+ branch = repo.branch in
    Printf.printf "Resolving for %s/%s\n" repo.owner repo.name;
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
    | Some (pr, value) -> (Some pr, value)
end

module Run = struct
  module Raw = struct
    type t = {
      mirage_dev : Git.Commit_id.t Current.t option;
      config : Config.t;
      platform : Platform.t;
      mirage_skeleton : Git.Commit_id.t Current.t;
      mirage : Git.Commit_id.t option Current.t;
      repos : Repository.t list Current.t;
      build_mode : Git.Commit_id.t Current.t Skeleton.build_mode;
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
      let open Current.Syntax in
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
      kind : string;
      commit_status : bool;
    }

    let perform_test_and_report_status
        { raw = { platform; _ } as raw; kind; gh_commit; commit_status } =
      let open Current.Syntax in
      let pipeline = Raw.perform_test raw in
      let commit_status =
        let id =
          let+ gh_commit' = gh_commit in
          Fmt.str "%s-%s"
            (Github.Api.Commit.id gh_commit' |> Git.Commit_id.hash)
            (Platform.platform_id platform)
        in
        match commit_status with
        | false -> Current_web_pipelines.Task.current pipeline
        | true ->
            let status =
              let+ state =
                Current_web_pipelines.Task.current pipeline
                |> Current.state ~hidden:true
              and+ id = id in
              github_status_of_state kind id state
            in
            Github.Api.Commit.set_status gh_commit
              (Fmt.str "Mirage CI - %a" Platform.pp_platform platform)
              status
      in
      let open Current_web_pipelines in
      let state = Task.state pipeline in
      (* OK because commit_status is derived from pipeline *)
      Task.v ~current:commit_status ~state
  end

  module Pipeline = struct
    type kind =
      | Mirage of {
          mirage_dev : Repo.t option;
          mirage_skeleton : Repo.t;
          build_mode : Repo.t Skeleton.build_mode;
        }
      | Mirage_dev of {
          mirage : Repo.t;
          mirage_skeleton : Repo.t;
          build_mode : Repo.t Skeleton.build_mode;
        }
      | Mirage_skeleton of {
          mirage : Repo.t;
          mirage_dev : Repo.t option;
          build_mode : Repo.t Skeleton.build_mode;
        }
      | Opam_overlays of {
          mirage : Repo.t;
          mirage_dev : Repo.t option;
          mirage_skeleton : Repo.t;
        }

    type github = {
      config : Config.t;
      repos : Repository.t list Current.t;
      repo : Repo.t;
      kind : kind;
      commit_status : bool;
    }

    type local = {
      config : Config.t;
      repos : Repository.t list Current.t;
      mirage : Git.Commit_id.t Current.t;
      mirage_skeleton : Git.Commit_id.t Current.t;
      mirage_dev : Git.Commit_id.t Current.t option;
      build_mode : Git.Commit_id.t Current.t Skeleton.build_mode;
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
            mirage = Current.map Option.some mirage;
            build_mode;
          }
      in
      let state =
        let open Current.Syntax in
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

    (* optional project *)
    let friend_pr_opt ~friends mirage_dev =
      Option.map (Friend_PR.resolve friends) mirage_dev |> fun a ->
      ( Option.map (Current.map fst) a
        |> Current.option_seq
        |> Current.map (fun v -> Option.bind v Fun.id),
        Option.map (Current.map snd) a )

    (* normal *)
    let friend_pr ~friends mirage_skeleton =
      Friend_PR.resolve friends mirage_skeleton |> fun a ->
      (Current.map fst a, Current.map snd a)

    (* optional resolution *)
    let friend_pr_resolve_opt ~friends mirage_skeleton =
      Friend_PR.resolve friends mirage_skeleton |> fun value ->
      let value =
        let open Current.Syntax in
        let+ friend_pr, value = value in
        match friend_pr with
        | None -> (None, None)
        | Some _ -> (friend_pr, Some value)
      in
      (Current.map fst value, Current.map snd value)

    (* optional overlay *)
    let friend_pr_build_mode ~friends = function
      | Skeleton.Mirage_3 -> (Current.return None, Skeleton.Mirage_3)
      | Mirage_4 { overlay } ->
          let friend_pr, value = friend_pr_opt ~friends overlay in
          (friend_pr, Mirage_4 { overlay = value })

    (* merge friend pr list *)

    let friend_pr_merge lst =
      Current.list_seq lst
      |> Current.map (fun v -> v |> List.map Option.to_list |> List.flatten)

    let id_of gh_commit = Current.map Github.Api.Commit.id gh_commit
    let wrap (a, b) = (a, fun _ -> b)

    let build_mode = function
      | Mirage { build_mode = Skeleton.Mirage_3; _ }
      | Mirage_dev { build_mode = Skeleton.Mirage_3; _ }
      | Mirage_skeleton { build_mode = Skeleton.Mirage_3; _ } ->
          `Mirage_3
      | _ -> `Mirage_4

    let resolve_build_mode ~friends = function
      | Mirage { build_mode; _ }
      | Mirage_dev { build_mode; _ }
      | Mirage_skeleton { build_mode; _ } ->
          wrap (friend_pr_build_mode ~friends build_mode)
      | Opam_overlays _ ->
          ( Current.return None,
            fun commit -> Skeleton.Mirage_4 { overlay = Some (id_of commit) } )

    let resolve_mirage ~friends = function
      | Opam_overlays { mirage; _ }
      | Mirage_dev { mirage; _ }
      | Mirage_skeleton { mirage; _ } ->
          wrap (friend_pr_resolve_opt ~friends mirage)
      | Mirage _ ->
          ( Current.return None,
            fun commit -> id_of commit |> Current.map Option.some )

    let resolve_mirage_dev ~friends = function
      | Opam_overlays { mirage_dev; _ }
      | Mirage { mirage_dev; _ }
      | Mirage_skeleton { mirage_dev; _ } ->
          wrap (friend_pr_opt ~friends mirage_dev)
      | Mirage_dev _ ->
          (Current.return None, fun commit -> id_of commit |> Option.some)

    let resolve_mirage_skeleton ~friends = function
      | Opam_overlays { mirage_skeleton; _ }
      | Mirage { mirage_skeleton; _ }
      | Mirage_dev { mirage_skeleton; _ } ->
          wrap (friend_pr ~friends mirage_skeleton)
      | Mirage_skeleton _ -> (Current.return None, id_of)

    let kind_name = function
      | Opam_overlays _ -> "opam-overlays"
      | Mirage _ -> "mirage"
      | Mirage_dev _ -> "mirage-dev"
      | Mirage_skeleton _ -> "mirage-skeleton"

    let github { kind; repo; commit_status; repos; config } =
      let perform_test ~friends =
        let friend_pr_mirage_dev, mirage_dev =
          resolve_mirage_dev ~friends kind
        in
        let friend_pr_mirage_skeleton, mirage_skeleton =
          resolve_mirage_skeleton ~friends kind
        in
        let friend_pr_mirage, mirage = resolve_mirage ~friends kind in
        let friend_pr_build_mode, build_mode =
          resolve_build_mode ~friends kind
        in
        ( friend_pr_merge
            [
              friend_pr_mirage;
              friend_pr_mirage_dev;
              friend_pr_mirage_skeleton;
              friend_pr_build_mode;
            ],
          fun ~platform commit ->
            let mirage = mirage commit in
            let mirage_skeleton = mirage_skeleton commit in
            let mirage_dev = mirage_dev commit in
            let build_mode = build_mode commit in
            Gh.perform_test_and_report_status
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
                kind = kind_name kind;
                commit_status;
              } )
      in
      repo.Repo.ci
      |> Current_web_pipelines.Task.list_iter ~collapse_key:"pipelines"
           (module Commit)
           (fun commit ->
             let ref = Current.map fst commit in
             let commit = Current.map snd commit in
             let friends = Current.map Friend_PR.find_friend_prs ref in
             let friend_prs, perform_test = perform_test ~friends in
             let pipeline =
               Platform.[ platform_v413_amd64; platform_v413_arm64 ]
               |> List.map (fun platform -> perform_test ~platform commit)
               |> Current_web_pipelines.Task.all
             in
             let state =
               let open Current.Syntax in
               let+ friend_prs = friend_prs
               and+ ref = ref
               and+ stages = Current_web_pipelines.Task.state pipeline
               and+ commit = commit in
               {
                 Current_web_pipelines.State.stages;
                 metadata =
                   {
                     ref;
                     friend_prs;
                     kind =
                       (match kind with
                       | Mirage _ -> `Mirage
                       | Mirage_dev _ -> `Mirage_dev
                       | Mirage_skeleton _ -> `Mirage_skeleton
                       | Opam_overlays _ -> `Opam_overlays);
                     owner = repo.owner;
                     name = repo.name;
                     commit = Github.Api.Commit.id commit |> Git.Commit_id.hash;
                     build_mode = build_mode kind;
                   };
               }
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
          | `Overlay -> { acc with dev = true })
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
  repos : Repository.t list Current.t;
}

let pipeline ~mirage ~mirage_skeleton ~mirage_dev ~build_mode
    { config; enable_commit_status; repos } =
  let pipelines =
    [
      {
        Run.Pipeline.config;
        repos;
        kind = Mirage { mirage_skeleton; mirage_dev; build_mode };
        repo = mirage;
        commit_status = enable_commit_status.mirage;
      };
      {
        Run.Pipeline.config;
        repos;
        kind = Mirage_skeleton { mirage; mirage_dev; build_mode };
        repo = mirage_skeleton;
        commit_status = enable_commit_status.skeleton;
      };
    ]
    @ (match mirage_dev with
      | Some mirage_dev ->
          [
            {
              Run.Pipeline.config;
              repos;
              kind = Mirage_dev { mirage; mirage_skeleton; build_mode };
              repo = mirage_dev;
              commit_status = enable_commit_status.dev;
            };
          ]
      | None -> [])
    @
    match build_mode with
    | Mirage_4 { overlay = None } -> []
    | Mirage_4 { overlay = Some i } ->
        [
          {
            Run.Pipeline.config;
            repos;
            kind = Opam_overlays { mirage; mirage_skeleton; mirage_dev };
            repo = i;
            commit_status = enable_commit_status.overlay;
          };
        ]
    | Mirage_3 -> []
  in
  List.map Run.Pipeline.github pipelines
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state List.flatten

type repo = { org : string; name : string; branch : string }

type test_set = {
  enable_commit_status : enable_commit_status;
  mirage : repo;
  mirage_skeleton : repo;
  mirage_dev : repo option;
  build_mode : repo Skeleton.build_mode;
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
                    {
                      org = "mirage";
                      name = "opam-overlays";
                      branch = "master";
                    };
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

let build_mode_map fn = function
  | Skeleton.Mirage_3 -> Skeleton.Mirage_3
  | Mirage_4 { overlay } -> Mirage_4 { overlay = (Option.map fn) overlay }

(* WE PERFORM TWO SETS OF TESTS
   - mirage skeleton 'master' / mirage '3' / mirage-dev '3'
   - mirage skeleton 'mirage-dev' / mirage 'main' / mirage-dev 'master' *)
let make ~config ~options ~repos github =
  let github_setup { branch; org; name } =
    Repo.github_setup ~branch ~github org name
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
         let build_mode = build_mode_map github_setup build_mode in
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
         let build_mode = build_mode_map github_setup build_mode in
         (* same pipeline, three sets of metadata. *)
         Run.Pipeline.local
           { config; repos; mirage_dev; mirage_skeleton; mirage; build_mode })
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state List.flatten
  |> Current_web_pipelines.Task.map_state
       (List.map (fun (t : _ Current_web_pipelines.State.pipeline) ->
            { t with metadata = `Local t.metadata }))
