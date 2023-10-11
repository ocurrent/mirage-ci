module Git = Current_git
open Common

let () = Logging_local.init ()
let program_name = "mirage-ci"
let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let main current_config mode config
    (`Pipelines_options mirage_pipelines_options) =
  let config = Common.Config.make config in

  let website = Website.make () in

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
      ~repos:(Opam_repository.current_list_unfetch repos)
  in

  let engine =
    Current.Engine.create ~config:current_config (fun () ->
        let current = Current_web_pipelines.Task.current main_ci in
        let state =
          let state_active =
            Current_web_pipelines.Task.state main_ci
            |> Current.map
                 (List.map (fun v -> v.Current_web_pipelines.State.metadata))
            |> Website.set_active_sources website
          in
          Current.list_iter ~collapse_key:"update-web-state"
            (module struct
              type t = Website.pipeline_state

              let pp f { Current_web_pipelines.State.metadata; _ } =
                Fmt.pf f "%s" (Website.Website_description.Pipeline.id metadata)

              let compare = Stdlib.compare
            end)
            (Website.update_state website)
            (Current_web_pipelines.Task.state main_ci)
          |> Current.pair state_active
          |> Current.map ignore
        in
        [ ("mirage-main-ci", current); ("mirage-state", state) ]
        |> Current.all_labelled)
  in
  let site =
    let routes = Current_web.routes engine @ Website.routes website engine in
    Current_web.Site.(v ~has_role:Current_web.Site.allow_all)
      ~name:program_name routes
  in
  Logging_local.run
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

let cmd =
  let doc = "an OCurrent pipeline" in
  let term =
    Term.(
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Common.Config.cmdliner
      $ main_ci)
  in
  let info = Cmd.info program_name ~doc in
  Cmd.v info (Term.term_result term)

let () = exit @@ Cmd.eval cmd
