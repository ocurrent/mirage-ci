module Github = Current_github
module Git = Current_git

type pr_info = { id : string; label : string; pipeline : unit Current.t }

type spec = { name : string; content : pr_info list ref }

type t = { specs : spec list; pipeline : unit Current.t }

type gh_repo = {
  ci : (Github.Api.Ref.t * Github.Api.Commit.t) list Current.t;
  branch : Github.Api.Commit.t Current.t;
}

let repo_refs ~github repo =
  let refs = Github.Api.refs github repo in
  Current.primitive ~info:(Current.component "repository refs") (fun () -> refs) (Current.return ())

let github_setup ~branch ~github owner name =
  let ref_filter = function
    | `PR (_, name) when name = branch -> true
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
        Api.Ref_map.filter (fun _ commit' -> Api.Commit.(hash commit' = hash commit)) map
        |> Api.Ref_map.bindings
        |> List.find_map (fun (ref, _) -> if ref_filter ref then Some (ref, commit) else None))
      ci_refs
  in
  let branch = Github.Api.head_of github gh (`Ref ("refs/heads/" ^ branch)) in
  { ci; branch }

let url kind id = Uri.of_string (Fmt.str "https://ci.mirage.io/github/%s/prs/%s" kind id)

let github_status_of_state kind id status =
  let url = url kind id in
  match status with
  | Ok _ -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m) -> Github.Api.Status.v ~url `Failure ~description:m

let perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos kind gh_commit =
  let open Current.Syntax in
  let repos =
    let+ repos = repos and+ mirage_dev = mirage_dev in
    ("mirage-dev", mirage_dev) :: repos
  in
  let* gh_commit' = gh_commit in
  let id =
    Fmt.str "%s-%s"
      (Github.Api.Commit.id gh_commit' |> Git.Commit_id.hash)
      (Mirage_ci_lib.Platform.platform_id platform)
  in
  let pipeline = Skeleton.v_main ~platform ~mirage ~repos mirage_skeleton in
  let result =
    Current.return { pipeline; label = Fmt.str "%a" Github.Api.Commit.pp gh_commit'; id }
  in
  let+ _ =
    match Mirage_ci_lib.Config.v.enable_commit_status with
    | false -> pipeline
    | true ->
        pipeline |> Current.state ~hidden:true
        |> Current.map (github_status_of_state kind id)
        |> Github.Api.Commit.set_status gh_commit
             (Fmt.str "Mirage CI - %a" Mirage_ci_lib.Platform.pp_platform platform)
  and+ result = result in
  result

let update lst value =
  let open Current.Syntax in
  let+ value = value in
  lst := List.flatten value

module CommitUrl = struct
  type t = Github.Api.Commit.t * (string * string)

  let pp f (_, (text, _)) = Fmt.pf f "%s" text

  let url f (_, (_, url)) = Fmt.pf f "%s" url

  let compare (a, _) (b, _) = Github.Api.Commit.compare a b
end

let pp_url ~(repo : Github.Repo_id.t) f ref =
  match ref with
  | `Ref ref -> Fmt.pf f "https://github.com/%s/%s/tree/%s" repo.owner repo.name ref
  | `PR (pr, _) -> Fmt.pf f "https://github.com/%s/%s/pull/%d" repo.owner repo.name pr

let url_of_commit (commit : Github.Api.Commit.t) (ref : Github.Api.Ref.t) =
  let open Github in
  let repo = Api.Commit.repo_id commit in
  (Fmt.str "Github: %a" Api.Ref.pp ref, Fmt.to_to_string (pp_url ~repo) ref)

type all_but_mirage = {
  mirage_dev : Git.Commit_id.t Current.t;
  mirage_skeleton : Git.Commit_id.t Current.t;
}

type all_but_mirage_dev = {
  mirage : Git.Commit_id.t Current.t;
  mirage_skeleton : Git.Commit_id.t Current.t;
}

type all_but_mirage_skeleton = {
  mirage : Git.Commit_id.t Current.t;
  mirage_dev : Git.Commit_id.t Current.t;
}

type kind =
  | Mirage of all_but_mirage
  | Mirage_dev of all_but_mirage_dev
  | Mirage_skeleton of all_but_mirage_skeleton

let id_of gh_commit = Current.map Github.Api.Commit.id gh_commit

let perform_ci ~name ~repos ~kind ci_refs =
  let perform_test =
    match kind with
    | Mirage { mirage_dev; mirage_skeleton } ->
        fun ~platform commit_mirage ->
          let mirage = id_of commit_mirage in
          perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos name commit_mirage
    | Mirage_dev { mirage; mirage_skeleton } ->
        fun ~platform commit_mirage_dev ->
          let mirage_dev = id_of commit_mirage_dev in
          perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos name commit_mirage_dev
    | Mirage_skeleton { mirage_dev; mirage } ->
        fun ~platform commit_mirage_skeleton ->
          let mirage_skeleton = id_of commit_mirage_skeleton in
          perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos name
            commit_mirage_skeleton
  in
  ci_refs
  |> Current.map (fun commits ->
         List.map (fun (ref, commit) -> (commit, url_of_commit commit ref)) commits)
  |> Current.list_map_url
       (module CommitUrl)
       (fun commit ->
         let commit = Current.map fst commit in
         Mirage_ci_lib.Platform.[ platform_amd64; platform_arm64 ]
         |> List.map (fun platform ->
                perform_test ~platform commit
                |> Current.collapse
                     ~key:(Fmt.str "%a" Mirage_ci_lib.Platform.pp_platform platform)
                     ~value:"mirage-skeleton" ~input:commit)
         |> Current.list_seq)

module Test = struct
  type t = { name : string; kind : kind; input : gh_repo }

  let v name kind input = { name; kind; input }
end

(* WE PERFORM TWO SETS OF TESTS
- mirage skeleton 'master' / mirage '3'
- mirage skeleton 'mirage-dev' / mirage 'master' / mirage-dev 'master'  *)
let make github repos =
  let gh_mirage_skeleton_master =
    github_setup ~branch:"master" ~github "mirage" "mirage-skeleton"
  in
  let gh_mirage_skeleton_dev =
    github_setup ~branch:"mirage-dev" ~github "mirage" "mirage-skeleton"
  in
  let gh_mirage_master = github_setup ~branch:"master" ~github "mirage" "mirage" in
  let gh_mirage_3 = github_setup ~branch:"3" ~github "mirage" "mirage" in
  let gh_mirage_dev = github_setup ~branch:"master" ~github "mirage" "mirage-dev" in
  let mirage_skeleton_master = id_of gh_mirage_skeleton_master.branch in
  let mirage_skeleton_dev = id_of gh_mirage_skeleton_dev.branch in
  let mirage_master = id_of gh_mirage_master.branch in
  let mirage_3 = id_of gh_mirage_3.branch in
  let mirage_dev = id_of gh_mirage_dev.branch in
  let with_context f =
    Current.with_context mirage_skeleton_master @@ fun () ->
    Current.with_context mirage_skeleton_dev @@ fun () ->
    Current.with_context mirage_master @@ fun () ->
    Current.with_context mirage_3 @@ fun () -> Current.with_context mirage_dev f
  in
  let pipeline =
    Test.
      [
        v "skeleton-master"
          (Mirage_skeleton { mirage = mirage_3; mirage_dev })
          gh_mirage_skeleton_master;
        v "skeleton-dev"
          (Mirage_skeleton { mirage = mirage_master; mirage_dev })
          gh_mirage_skeleton_dev;
        v "mirage-master"
          (Mirage { mirage_skeleton = mirage_skeleton_dev; mirage_dev })
          gh_mirage_master;
        v "mirage-3" (Mirage { mirage_skeleton = mirage_skeleton_master; mirage_dev }) gh_mirage_3;
        v "mirage-dev"
          (Mirage_dev { mirage_skeleton = mirage_skeleton_master; mirage = mirage_master })
          gh_mirage_dev;
      ]
    |> List.map (fun Test.{ name; kind; input } ->
           let prs = ref [] in
           ( name,
             prs,
             with_context @@ fun () -> perform_ci ~name ~repos ~kind input.ci |> update prs ))
  in
  let specs = List.map (fun (name, content, _) -> { name; content }) pipeline in
  let pipeline = (List.map (fun (a, _, b) -> (a, b))) pipeline |> Current.all_labelled in
  { pipeline; specs }

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
          div ~a:[ a_id "pipeline_container" ]
            [
              div ~a:[ a_id "pipeline" ] [ html ]; Unsafe.data "<iframe id='logs_iframe' ></iframe>";
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
