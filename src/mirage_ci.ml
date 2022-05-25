open Common
module Github = Current_github
module Git = Current_git
module Docker = Current_docker.Default

let () = Logging.init ()
let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

(* Access control policy. *)
let has_role user role =
  match user with
  | None ->
      role = `Viewer || role = `Monitor
      (* Unauthenticated users can only look at things. *)
  | Some user -> (
      match (Current_web.User.id user, role) with
      | "github:TheLortex", _ -> true (* These users have all roles *)
      | _ -> role = `Viewer)

let program_name = "mirage-ci"

let main current_config github mode auth config
    (`Pipelines_options mirage_pipelines_options) (`Self_deploy self_deploy) =
  let config = Common.Config.make config in
  let website = Website.make () in
  let prs =
    match github with
    | None when Mirage_ci_pipelines.PR.is_enabled mirage_pipelines_options ->
        Logs.err (fun f ->
            f
              "No github API key was provided using the github-token-file \
               option !");
        None
    | None -> None
    | Some github ->
        let repo_opam =
          Current_git.clone ~schedule:daily
            "https://github.com/ocaml/opam-repository.git"
        in
        let repos =
          let open Current.Syntax in
          let+ repo_opam = repo_opam in
          [ ("opam", repo_opam) ]
        in
        Some
          (Mirage_ci_pipelines.PR.make ~config ~options:mirage_pipelines_options
             ~repos:(Opam_repository.current_list_unfetch repos)
             github)
  in
  let main_ci =
    match prs with
    | None -> []
    | Some prs ->
        let current = Current_web_pipelines.Task.current prs in
        let state =
          let input = Current_web_pipelines.Task.state prs in
          Current.list_iter ~collapse_key:"update-web-state"
            (module struct
              type t = Website.pipeline_state

              let pp f { Current_web_pipelines.State.metadata; _ } =
                Fmt.pf f "%s" (Website.Website_description.Pipeline.id metadata)

              let compare = Stdlib.compare
            end)
            (Website.update_state website)
            input
          |> Current.collapse ~key:"current-web-pipeline-internals" ~value:""
               ~input:
                 (Current.return ~label:"current-web-pipelines internals" ())
        in
        [ ("mirage-main-ci", current); ("mirage-state", state) ]
  in

  let self_deploy =
    if self_deploy then
      let commit =
        Github.Api.Anonymous.head_of
          { Github.Repo_id.owner = "ocurrent"; name = "mirage-ci" }
          (`Ref "refs/heads/live")
        |> Current_git.fetch
      in
      let image = Docker.build ~pull:false (`Git commit) in
      [ ("self-deploy", Docker.service ~name:"infra_mirage-ci" ~image ()) ]
    else []
  in

  let engine =
    Current.Engine.create ~config:current_config (fun () ->
        Current.all_labelled (main_ci @ self_deploy))
  in
  let has_role = if auth = None then Current_web.Site.allow_all else has_role in
  let webhook_secret =
    match github with
    | Some api -> Current_github.Api.webhook_secret api
    | None -> ""
  in
  let site =
    let routes =
      Routes.((s "login" /? nil) @--> Current_github.Auth.login auth)
      :: Routes.(
           (s "webhooks" / s "github" /? nil)
           @--> Github.webhook ~engine ~webhook_secret ~has_role)
      :: Current_web.routes engine
      @ Website.routes website
    in
    Current_web.Site.(v ~has_role) ~name:program_name routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site (* Optional: provides a web UI *);
       ])

(* Command-line parsing *)

open Cmdliner

let named f = Cmdliner.Term.(app (const f))

let main_ci =
  Mirage_ci_pipelines.PR.test_options_cmdliner
  |> named (fun x -> `Pipelines_options x)

let github_config =
  let read_file path =
    let ch = open_in_bin path in
    Fun.protect
      (fun () ->
        let len = in_channel_length ch in
        really_input_string ch len)
      ~finally:(fun () -> close_in ch)
  in
  let token =
    Arg.value
    @@ Arg.opt Arg.(some file) None
    @@ Arg.info ~doc:"A file containing the GitHub OAuth token." ~docv:"PATH"
         [ "github-token-file" ]
  in
  let webhook_secret =
    Arg.value
    @@ Arg.opt Arg.(some file) None
    @@ Arg.info ~doc:"A file containing the GitHub webhook secret." ~docv:"PATH"
         [ "github-webhook-secret-file" ]
  in
  Term.(
    const (fun x y ->
        match (x, y) with
        | Some token, Some webhook_secret ->
            Some
              (Current_github.Api.of_oauth
                 ~token:(String.trim (read_file token))
                 ~webhook_secret:(String.trim (read_file webhook_secret)))
        | _ -> None)
    $ token
    $ webhook_secret)

let self_deploy =
  Arg.value
  @@ Arg.flag
  @@ Arg.info
       ~doc:
         "Self deploy https://github.com/ocurrent/mirage-ci to infra_mirage-ci \
          service"
       [ "self-deploy" ]
  |> named (fun x -> `Self_deploy x)

let cmd =
  let doc = "an OCurrent pipeline" in
  let term =
    Term.(
      const main
      $ Current.Config.cmdliner
      $ github_config
      $ Current_web.cmdliner
      $ Current_github.Auth.cmdliner
      $ Common.Config.cmdliner
      $ main_ci
      $ self_deploy)
  in
  let info = Cmd.info program_name ~doc in
  Cmd.v info (Term.term_result term)

let () = exit @@ Cmd.eval cmd
