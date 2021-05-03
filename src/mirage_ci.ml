open Mirage_ci_lib
module Github = Current_github
module Git = Current_git
open Current.Syntax

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let program_name = "mirage-ci"

let gh_mirage_skeleton = { Github.Repo_id.owner = "mirage"; name = "mirage-skeleton" }

let gh_mirage_dev = { Github.Repo_id.owner = "mirage"; name = "mirage-dev" }

let main config github mode (`Ocluster_cap cap) (`Monorepo_pull remote_pull)
    (`Monorepo_push remote_push) (`Enable_mirage_4 enable_mirage_4) (`PR_CI pr_ci)
    (`Commit_status commit_status) =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let submission_cap = Capnp_rpc_unix.Vat.import_exn vat cap in
  let connection = Current_ocluster.Connection.create ~max_pipeline:20 submission_cap in
  let ocluster = Current_ocluster.v ~urgent:`Never connection in

  let repo_mirage_skeleton =
    let+ repo_gh = Github.Api.head_of github gh_mirage_skeleton (`Ref "refs/heads/mirage-4") in
    Github.Api.Commit.id repo_gh
  in
  let repo_mirage_skeleton = Git.fetch repo_mirage_skeleton in
  let repo_mirage_dev =
    let+ repo_gh = Github.Api.head_of github gh_mirage_dev (`Ref "refs/heads/mirage-4") in
    Github.Api.Commit.id repo_gh
  in
  let repo_mirage_dev = Git.fetch repo_mirage_dev in
  let repo_opam =
    Current_git.clone ~schedule:daily "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily "https://github.com/dune-universe/opam-overlays.git"
  in
  let repos =
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
      repo_mirage_dev |> Current.map (fun x -> ("mirage-dev", x));
    ]
    |> Current.list_seq
  in
  let repos_unfetched = Repository.current_list_unfetch repos in
  let repos_mirage_main =
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
    ]
    |> Current.list_seq
  in
  let roots = Universe.Project.packages in
  let monorepo = Monorepo.v ~system:Platform.system ~repos in
  let monorepo_lock =
    Mirage_ci_pipelines.Monorepo.lock ~system:Platform.system ~value:"universe" ~monorepo
      ~repos:repos_unfetched roots
  in
  let mirage_4 =
    if enable_mirage_4 then
      Current.with_context repos @@ fun () ->
      let mirage_skeleton_arm64 =
        Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_arm64
          ~targets:[ "unix"; "hvt" ] ~monorepo ~repos repo_mirage_skeleton
      in
      let mirage_skeleton_amd64 =
        Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_amd64 ~targets:[ "xen"; "spt" ]
          ~monorepo ~repos repo_mirage_skeleton
      in
      let mirage_released =
        Mirage_ci_pipelines.Monorepo.released ~platform:Platform.platform_arm64 ~roots
          ~repos:repos_unfetched ~lock:monorepo_lock
      in
      let mirage_edge =
        Mirage_ci_pipelines.Monorepo.mirage_edge ~platform:Platform.platform_arm64 ~remote_pull
          ~remote_push ~roots ~repos:repos_unfetched ~lock:monorepo_lock
      in
      let universe_edge =
        Mirage_ci_pipelines.Monorepo.universe_edge ~platform:Platform.platform_arm64 ~remote_pull
          ~remote_push ~roots ~repos:repos_unfetched ~lock:monorepo_lock
      in
      Current.all_labelled
        [
          ("mirage-skeleton-arm64", mirage_skeleton_arm64 ~ocluster);
          ("mirage-skeleton-amd64", mirage_skeleton_amd64 ~ocluster);
          ("mirage-released", mirage_released ~ocluster);
          ("mirage-edge", mirage_edge ~ocluster);
          ("universe-edge", universe_edge ~ocluster);
        ]
    else Current.return ~label:"disabled" ()
  in
  let prs =
    Mirage_ci_pipelines.PR.make ~ocluster ~test:pr_ci ~commit_status github
      (Repository.current_list_unfetch repos_mirage_main)
  in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled
          [ ("mirage 4", mirage_4); ("mirage-main-ci", Mirage_ci_pipelines.PR.to_current prs) ])
  in
  let site =
    let routes =
      Routes.((s "webhooks" / s "github" /? nil) @--> Github.webhook)
      :: Mirage_ci_pipelines.PR.routes prs
      @ Current_web.routes engine
    in
    Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes
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

let monorepo_pull =
  Arg.required
  @@ Arg.(opt (some string) None)
  @@ Arg.info ~doc:"Repository from which workers can pull monorepos" [ "monorepo-pull-from" ]
  |> named (fun x -> `Monorepo_pull x)

let monorepo_push =
  Arg.required
  @@ Arg.(opt (some string) None)
  @@ Arg.info ~doc:"Repository to which main node can push" [ "monorepo-push-to" ]
  |> named (fun x -> `Monorepo_push x)

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

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main $ Current.Config.cmdliner $ Current_github.Api.cmdliner $ Current_web.cmdliner
      $ ocluster_cap $ monorepo_pull $ monorepo_push $ mirage_4 $ main_ci_jobs
      $ main_ci_commit_status),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
