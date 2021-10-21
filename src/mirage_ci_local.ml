module Git = Current_git
open Common

let () = Logging_local.init ()
let program_name = "mirage-ci"
let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let main current_config mode config
    (`Pipelines_options mirage_pipelines_options) =
  let config = Common.Config.make config in
  let main_ci =
    let repo_opam =
      Current_git.clone ~schedule:daily
        "https://github.com/ocaml/opam-repository.git"
    in
    let repos =
      let open Current.Syntax in
      let+ repo_opam = repo_opam in
      [ ("opam", repo_opam) ]
    in
    Mirage_ci_pipelines.PR.local ~config ~options:mirage_pipelines_options
      ~repos:(Repository.current_list_unfetch repos)
  in

  let engine =
    Current.Engine.create ~config:current_config (fun () -> main_ci)
  in
  let site =
    let routes = Current_web.routes engine in
    Current_web.Site.(v ~has_role:Current_web.Site.allow_all)
      ~name:program_name routes
  in
  Logging_local.run
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

let main_ci =
  Mirage_ci_pipelines.PR.test_options_cmdliner
  |> named (fun x -> `Pipelines_options x)

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Common.Config.cmdliner
      $ main_ci),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
