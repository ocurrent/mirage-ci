open Mirage_ci_lib
module Github = Current_github
module Git = Current_git

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

(* Access control policy. *)
let has_role user role =
  match user with
  | None -> role = `Viewer || role = `Monitor (* Unauthenticated users can only look at things. *)
  | Some user -> (
      match (Current_web.User.id user, role) with
      | "github:TheLortex", _ -> true (* These users have all roles *)
      | _ -> role = `Viewer )

let program_name = "mirage-ci"

let gh_mirage_dev = { Github.Repo_id.owner = "mirage"; name = "mirage-dev" }

let gh_head_of github name ref =
  match github with
  | None -> Github.Api.Anonymous.head_of name ref
  | Some github -> Github.Api.head_of github name ref |> Current.map Current_github.Api.Commit.id

let main config github mode auth store (`Ocluster_cap cap) (`Enable_mirage_4 enable_mirage_4)
    (`PR_CI pr_ci) (`Commit_status commit_status) =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat cap in
  let connection = Current_ocluster.Connection.create ~max_pipeline:20 submission_cap in
  let ocluster =
    Current_ocluster.v ~secrets:(Git_store.Cluster.secrets store) ~urgent:`Never connection
  in

  let repo_mirage_dev = gh_head_of github gh_mirage_dev (`Ref "refs/heads/master") in
  let repo_mirage_dev = Git.fetch repo_mirage_dev in
  let repo_opam =
    Current_git.clone ~schedule:daily "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily "https://github.com/mirage/opam-overlays.git"
  in
  let mirage_4 =
    if enable_mirage_4 then
      let repos =
        [
          repo_opam |> Current.map (fun x -> ("opam", x));
          repo_overlays |> Current.map (fun x -> ("overlays", x));
          repo_mirage_dev |> Current.map (fun x -> ("mirage-dev", x));
        ]
        |> Current.list_seq
      in
      let repos_unfetched = Repository.current_list_unfetch repos in
      let roots = Universe.Project.packages in
      let platform =
        match Config.profile with
        | `Docker -> Platform.platform_host
        | `Production | `Dev -> Platform.platform_v413_arm64
      in
      let monorepo = Monorepo.v ~system:platform.system ~repos in
      let monorepo_lock =
        Mirage_ci_pipelines.Monorepo.lock ~ocluster ~store ~system:platform.system ~value:"universe"
          ~monorepo ~repos:repos_unfetched roots
      in
      Current.with_context repos @@ fun () ->
      let mirage_released =
        Mirage_ci_pipelines.Monorepo.released ~platform ~roots ~repos:repos_unfetched
          ~lock:monorepo_lock
      in
      let mirage_edge =
        Mirage_ci_pipelines.Monorepo.mirage_edge ~platform ~git_store:store ~roots
          ~repos:repos_unfetched ~lock:monorepo_lock
      in
      let universe_edge =
        Mirage_ci_pipelines.Monorepo.universe_edge ~platform ~git_store:store ~roots
          ~repos:repos_unfetched ~lock:monorepo_lock
      in
      Current.all_labelled
        [
          ("mirage-released", mirage_released ~ocluster);
          ("mirage-edge", mirage_edge ~ocluster);
          ("universe-edge", universe_edge ~ocluster);
        ]
    else Current.return ~label:"disabled" ()
  in
  let prs =
    match github with
    | None when List.length pr_ci > 0 ->
        Logs.err (fun f -> f "No github API key was provided using the github-token-file option !");
        None
    | None -> None
    | Some github ->
        let repos_mirage_main =
          [
            repo_opam |> Current.map (fun x -> ("opam", x));
            repo_overlays |> Current.map (fun x -> ("overlays", x));
          ]
          |> Current.list_seq
        in
        Some
          (Mirage_ci_pipelines.PR.make ~ocluster ~test:pr_ci ~commit_status github
             (Repository.current_list_unfetch repos_mirage_main))
  in
  let main_ci, main_routes =
    match prs with
    | None -> ([], [])
    | Some prs ->
        ( [ ("mirage-main-ci", Mirage_ci_pipelines.PR.to_current prs) ],
          Mirage_ci_pipelines.PR.routes prs )
  in

  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled (("mirage 4", mirage_4) :: main_ci))
  in
  let has_role = if auth = None then Current_web.Site.allow_all else has_role in
  let site =
    let routes =
      Routes.((s "login" /? nil) @--> Current_github.Auth.login auth)
      :: Routes.((s "webhooks" / s "github" /? nil) @--> Github.webhook)
      :: main_routes
      @ Current_web.routes engine
    in
    Current_web.Site.(v ~has_role) ~name:program_name routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site;
         (* Optional: provides a web UI *)
       ])

(* Command-line parsing *)

open Cmdliner

let named f = Cmdliner.Term.(app (const f))

let ocluster_cap =
  Arg.required
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The ocluster submission capability file" ~docv:"FILE" [ "ocluster-cap" ]
  |> named (fun x -> `Ocluster_cap x)

let mirage_4 =
  Arg.value @@ Arg.flag
  @@ Arg.info ~doc:"Test `TheLortex/mirage#mirage-4` development branch" [ "test-mirage-4" ]
  |> named (fun x -> `Enable_mirage_4 x)

let pr_kind_conv =
  Arg.conv
    ( (function
      | "all" -> Ok Mirage_ci_pipelines.PR.RepoBranch.all
      | v ->
          Mirage_ci_pipelines.PR.RepoBranch.of_string v
          |> Option.map (fun x -> [ x ])
          |> Option.to_result ~none:(`Msg "failed to parse target")),
      Fmt.(list (using Mirage_ci_pipelines.PR.RepoBranch.to_string string)) )

let main_ci_jobs =
  Arg.value
  @@ Arg.(opt (list pr_kind_conv) [])
  @@ Arg.info ~doc:"Track PRs for the following jobs" [ "main-ci" ]
  |> named (fun x -> `PR_CI (List.flatten x))

let main_ci_commit_status =
  Arg.value
  @@ Arg.(opt (list pr_kind_conv) [])
  @@ Arg.info ~doc:"Report commit status for the following jobs" [ "commit-status" ]
  |> named (fun x -> `Commit_status (List.flatten x))

let github_config =
  let read_file path =
    let ch = open_in_bin path in
    Fun.protect
      (fun () ->
        let len = in_channel_length ch in
        really_input_string ch len)
      ~finally:(fun () -> close_in ch)
  in
  Arg.value
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"A file containing the GitHub OAuth token." ~docv:"PATH" [ "github-token-file" ]
  |> named (Option.map (fun x -> Current_github.Api.of_oauth @@ String.trim (read_file x)))

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main $ Current.Config.cmdliner $ github_config $ Current_web.cmdliner
      $ Current_github.Auth.cmdliner $ Git_store.cmdliner $ ocluster_cap $ mirage_4 $ main_ci_jobs
      $ main_ci_commit_status),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
