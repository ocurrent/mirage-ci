module Github = Current_github
module Git = Current_git
open Common

type pr_info = { id : string; label : string; pipeline : unit Current.t }
type spec = { name : string; content : pr_info list ref }
type t = { specs : spec list; pipeline : unit Current.t }

type gh_repo = {
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
           let org = Str.matched_group 1 content in
           let repo = Str.matched_group 2 content in
           let pr = Str.matched_group 3 content |> int_of_string in
           Some (org, repo, pr)
         else None)

let find_friend_prs = function
  | `PR Github.Api.Ref.{ bodyHTML; _ } -> find_friend_prs bodyHTML
  | _ -> []

let github_setup ~branch ~github owner name =
  let ref_filter = function
    | `PR Github.Api.Ref.{ base; _ } when base = branch -> true
    | `Ref ref when ref = "refs/heads/" ^ branch -> true
    | _ -> false
  in
  let open Current.Syntax in
  let open Github in
  let gh = { Github.Repo_id.owner; name } in
  let ci_refs = Github.Api.ci_refs ~staleness:(Duration.of_day 90) github gh in
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

let url kind id =
  Uri.of_string (Fmt.str "https://ci.mirage.io/github/%s/prs/%s" kind id)

let github_status_of_state kind id status =
  let url = url kind id in
  match status with
  | Ok _ -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m) -> Github.Api.Status.v ~url `Failure ~description:m

let perform_test ?mirage_dev ~config ~platform ~mirage_skeleton ~mirage ~repos
    ~build_mode () =
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

let perform_test_and_report_status ?mirage_dev ~config ~commit_status ~platform
    ~mirage_skeleton ~mirage ~repos ~build_mode kind gh_commit =
  let open Current.Syntax in
  let pipeline =
    perform_test ?mirage_dev ~config ~platform ~mirage_skeleton ~mirage ~repos
      ~build_mode ()
  in
  let* gh_commit' = gh_commit in
  let id =
    Fmt.str "%s-%s"
      (Github.Api.Commit.id gh_commit' |> Git.Commit_id.hash)
      (Platform.platform_id platform)
  in
  let result =
    Current.return
      { pipeline; label = Fmt.str "%a" Github.Api.Commit.pp gh_commit'; id }
  in
  let+ _ =
    match commit_status with
    | false -> pipeline
    | true ->
        pipeline
        |> Current.state ~hidden:true
        |> Current.map (github_status_of_state kind id)
        |> Github.Api.Commit.set_status gh_commit
             (Fmt.str "Mirage CI - %a" Platform.pp_platform platform)
  and+ result = result in
  result

let update lst value =
  let open Current.Syntax in
  let+ value = value in
  lst := List.flatten value

module CommitUrl = struct
  type t = (Github.Api.Commit.t * Github.Api.Ref.t) * (string * string)

  let pp f (_, (text, _)) = Fmt.pf f "%s" text
  let url f (_, (_, url)) = Fmt.pf f "%s" url
  let compare ((a, _), _) ((b, _), _) = Github.Api.Commit.compare a b
end

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

type kind =
  | Mirage of {
      mirage_dev : gh_repo option;
      mirage_skeleton : gh_repo;
      build_mode : gh_repo Skeleton.build_mode;
    }
  | Mirage_dev of {
      mirage : gh_repo;
      mirage_skeleton : gh_repo;
      build_mode : gh_repo Skeleton.build_mode;
    }
  | Mirage_skeleton of {
      mirage : gh_repo;
      mirage_dev : gh_repo option;
      build_mode : gh_repo Skeleton.build_mode;
    }
  | Opam_overlays of {
      mirage : gh_repo;
      mirage_dev : gh_repo option;
      mirage_skeleton : gh_repo;
    }

let id_of gh_commit = Current.map Github.Api.Commit.id gh_commit

let resolve_opt friends repo =
  let open Current.Syntax in
  let+ friends = friends and+ refs = repo.all in
  Printf.printf "Resolving for %s/%s\n" repo.owner repo.name;
  List.find_map
    (fun (owner, name, number) ->
      if repo.owner = owner && repo.name = name then (
        Printf.printf "looking for %d\n" number;
        List.find_map
          (function
            | `PR Github.Api.Ref.{ id; _ }, value when id = number ->
                Printf.printf "%s %s >  %d!!\n" owner name id;
                Some value
            | _ -> None)
          refs)
      else None)
    friends
  |> Option.map Github.Api.Commit.id

let resolve friends repo =
  let open Current.Syntax in
  let+ result = resolve_opt friends repo and+ branch = repo.branch in
  Option.value result ~default:(Github.Api.Commit.id branch)

let build_mode_map f = function
  | Skeleton.Mirage_3 -> Skeleton.Mirage_3
  | Mirage_4 { overlay } -> Mirage_4 { overlay = Option.map f overlay }

let resolve_build_mode friends = build_mode_map (resolve friends)

type test = {
  name : string;
  kind : kind;
  input : gh_repo;
  commit_status : bool;
}

let perform_ci ~config ~name ~commit_status ~repos ~kind ci_refs =
  let perform_test ~ref =
    let friends = Current.map find_friend_prs ref in
    match kind with
    | Mirage { mirage_dev; mirage_skeleton; build_mode } ->
        (* Testing mirage commits and PRs *)
        let mirage_dev = Option.map (resolve friends) mirage_dev in
        let mirage_skeleton = resolve friends mirage_skeleton in
        let build_mode = resolve_build_mode friends build_mode in
        fun ~platform commit_mirage ->
          let mirage = id_of commit_mirage |> Current.map Option.some in
          perform_test_and_report_status ~platform ?mirage_dev ~mirage_skeleton
            ~mirage ~repos ~build_mode name commit_mirage
    | Mirage_dev { mirage; mirage_skeleton; build_mode } ->
        (* Testing mirage-dev commits and PRs *)
        let mirage = resolve_opt friends mirage in
        (* we pin mirage only if we want to test with a PR on mirage *)
        let mirage_skeleton = resolve friends mirage_skeleton in
        let build_mode = resolve_build_mode friends build_mode in
        fun ~platform commit_mirage_dev ->
          let mirage_dev = id_of commit_mirage_dev in
          perform_test_and_report_status ~platform ~mirage_dev ~mirage_skeleton
            ~mirage ~repos ~build_mode name commit_mirage_dev
    | Mirage_skeleton { mirage_dev; mirage; build_mode } ->
        (* Testing mirage-skeleton commits and PRs *)
        let mirage_dev = Option.map (resolve friends) mirage_dev in
        let build_mode = resolve_build_mode friends build_mode in
        let mirage = resolve_opt friends mirage in
        (* we pin mirage only if we want to test with a PR on mirage *)
        fun ~platform commit_mirage_skeleton ->
          let mirage_skeleton = id_of commit_mirage_skeleton in
          perform_test_and_report_status ~platform ?mirage_dev ~mirage_skeleton
            ~mirage ~repos ~build_mode name commit_mirage_skeleton
    | Opam_overlays { mirage; mirage_skeleton; mirage_dev } ->
        (* Testing opam-overlays commits and PRs *)
        let mirage = resolve_opt friends mirage in
        (* we pin mirage only if we want to test with a PR on mirage *)
        let mirage_dev = Option.map (resolve friends) mirage_dev in
        let mirage_skeleton = resolve friends mirage_skeleton in
        fun ~platform commit_opam_overlays ->
          let overlay = Some (id_of commit_opam_overlays) in
          let build_mode = Skeleton.Mirage_4 { overlay } in
          perform_test_and_report_status ~platform ?mirage_dev ~mirage_skeleton
            ~mirage ~repos ~build_mode name commit_opam_overlays
  in
  ci_refs
  |> Current.map (fun commits ->
         List.map
           (fun (ref, commit) -> ((commit, ref), url_of_commit commit ref))
           commits)
  |> Current.list_map_url
       (module CommitUrl)
       (fun commit ->
         let ref = Current.map (fun ((_, r), _) -> r) commit in
         let commit = Current.map (fun ((c, _), _) -> c) commit in
         Platform.[ platform_v413_amd64; platform_v413_arm64 ]
         |> List.map (fun platform ->
                perform_test ~config ~commit_status ~ref ~platform commit
                |> Current.collapse
                     ~key:(Fmt.str "%a" Platform.pp_platform platform)
                     ~value:"mirage-skeleton" ~input:commit)
         |> Current.list_seq)

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
  let tasks =
    [
      {
        name = "mirage";
        kind = Mirage { mirage_skeleton; mirage_dev; build_mode };
        input = mirage;
        commit_status = enable_commit_status.mirage;
      };
      {
        name = "mirage-skeleton";
        kind = Mirage_skeleton { mirage; mirage_dev; build_mode };
        input = mirage_skeleton;
        commit_status = enable_commit_status.skeleton;
      };
    ]
    @ (match mirage_dev with
      | Some mirage_dev ->
          [
            {
              name = "mirage-dev";
              kind = Mirage_dev { mirage; mirage_skeleton; build_mode };
              input = mirage_dev;
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
            name = "opam-overlays";
            kind = Opam_overlays { mirage; mirage_skeleton; mirage_dev };
            input = i;
            commit_status = enable_commit_status.overlay;
          };
        ]
    | Mirage_3 -> []
  in
  let pipeline =
    tasks
    |> List.map (fun { name; kind; input; commit_status } ->
           let prs = ref [] in
           ( name,
             prs,
             perform_ci ~config ~name ~commit_status ~repos ~kind input.ci
             |> update prs ))
  in
  let specs = List.map (fun (name, content, _) -> { name; content }) pipeline in
  let pipeline =
    (List.map (fun (a, _, b) -> (a, b))) pipeline |> Current.all_labelled
  in
  (specs, pipeline)

type repo = { org : string; name : string; branch : string }

type test_set = {
  enable_commit_status : enable_commit_status;
  name : string;
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
          name = "mirage-4";
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
          name = "mirage-3";
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
    github_setup ~branch ~github org name
  in
  let specs, pipelines =
    tests options
    |> List.map
         (fun
           {
             enable_commit_status;
             name;
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
           let spec, pipeline =
             pipeline ~mirage ~mirage_skeleton ~mirage_dev ~build_mode ctx
           in
           (spec, (name, pipeline)))
    |> List.split
  in
  { pipeline = Current.all_labelled pipelines; specs = List.concat specs }

let local ~config ~options ~repos =
  let github_setup { branch; org; name } =
    Github.Api.Anonymous.head_of { owner = org; name }
      (`Ref ("refs/heads/" ^ branch))
  in
  let pipelines =
    tests options
    |> List.map
         (fun { name; mirage; mirage_dev; mirage_skeleton; build_mode; _ } ->
           let mirage = github_setup mirage |> Current.map Option.some in
           let mirage_skeleton = github_setup mirage_skeleton in
           let mirage_dev = Option.map github_setup mirage_dev in
           let build_mode = build_mode_map github_setup build_mode in
           ( name,
             perform_test ?mirage_dev ~config
               ~platform:Common.Platform.platform_host ~mirage_skeleton ~mirage
               ~repos ~build_mode () ))
  in
  Current.all_labelled pipelines

let to_current t = t.pipeline

open Current_web
open Tyxml.Html

let r (pr : pr_info) =
  object
    inherit Current_web.Resource.t
    val! can_get = `Viewer

    method! private get ctx =
      let pipeline = pr.pipeline in
      let job_info { Current.Metadata.job_id; update } =
        let url = job_id |> Option.map (fun id -> Fmt.str "/job/%s" id) in
        (update, url)
      in
      let html, _ = Current.Analysis.to_html_css ~job_info pipeline in
      Context.respond_ok ctx
        [
          style
            [
              Unsafe.data
                {|
      #pipeline_container {
        display: flex;
        flex-direction: row;
      }

      #logs_iframe {
        height: calc(100vh - 40px);
        flex: 1;
        border: none;
        border-left: solid gray 1px;
        padding-left: 10px;
        margin-left: 10px;
      }

      |};
            ];
          div
            ~a:[ a_id "pipeline_container" ]
            [
              div ~a:[ a_id "pipeline" ] [ html ];
              Unsafe.data "<iframe id='logs_iframe' ></iframe>";
            ];
          script
            (Unsafe.data
               {|
        let logs = document.getElementById("logs_iframe");

        function setLogsUrl(url) {
          logs.src = url;
        }
        |});
        ]

    method! nav_link = Some "New pipeline rendering"
  end

let route skeleton_prs pr_id =
  let pr = List.find (fun pr -> pr.id = pr_id) !skeleton_prs in
  (* TODO: what if not found*)
  r pr

let routes t =
  t.specs
  |> List.map @@ fun { name; content } ->
     Routes.((s "github" / s name / s "prs" / str /? nil) @--> route content)
