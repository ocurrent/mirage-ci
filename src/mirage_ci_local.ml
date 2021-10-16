module Git = Current_git
open Common

let () = Logging_local.init ()
let program_name = "mirage-ci"
let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()
let is_local repo = Sys.file_exists repo && Sys.is_directory repo

let clone name repo grefs =
  let gref = List.assoc_opt name grefs in
  if is_local repo then
    let info = Current.component "clone %s" repo in
    let repo = Current_git.Local.v (Fpath.v repo) in
    let id =
      match gref with
      | Some gref ->
          let gref = "refs/heads/" ^ gref in
          Current_git.Local.commit_of_ref repo gref
      | None -> Current_git.Local.head_commit repo
    in
    (* FIXME: that should be fixed in Current_git.Local *)
    let id = Current.bind ~info Current.return id in
    (name, id)
  else
    let gref = Option.map (fun r -> "refs/heads/" ^ r) gref in
    (name, Current_git.clone ~schedule:daily ?gref repo)

let main current_config mode config
    (`Pipelines_options mirage_pipelines_options)
    (`Opam_repository opam_repositoty) (`Opam_overlay opam_overlay)
    (`Mirage_skeleton mirage_skeleton) (`Mirage_dev mirage_dev) (`Mirage mirage)
    =
  let config = Common.Config.make config in
  let repos grefs =
    [
      clone "mirage" mirage grefs;
      clone "opam-repository" opam_repositoty grefs;
      clone "opam-overlays" opam_overlay grefs;
      clone "mirage-skeleton" mirage_skeleton grefs;
      clone "mirage-dev" mirage_dev grefs;
    ]
  in
  let main_ci =
    let repos grefs =
      repos grefs
      |> List.map (fun (m, c) -> Current.map (fun x -> (m, x)) c)
      |> Current.list_seq
      |> Repository.current_list_unfetch
    in
    Mirage_ci_pipelines.PR.local ~config ~options:mirage_pipelines_options repos
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
let docv = "REPOSITORY"

let opam_repository =
  let open Cmdliner in
  let doc =
    "The $(docv) pointing to the opam-repository. Can be a local repository."
  in
  let default = "https://github.com/ocaml/opam-repository.git" in
  named
    (fun x -> `Opam_repository x)
    Arg.(value & opt string default & info ~doc ~docv [ "opam-repository" ])

let opam_overlays =
  let open Cmdliner in
  let docv = "REPOSITORY" in
  let doc =
    "The $(docv) pointing to the opam-overlays. Can be a local repository."
  in
  let default = "https://github.com/mirage/opam-overlays.git" in
  named
    (fun x -> `Opam_overlay x)
    Arg.(value & opt string default & info ~doc ~docv [ "opam-overlays" ])

let mirage_skeleton =
  let open Cmdliner in
  let docv = "REPOSITORY" in
  let doc =
    "The $(docv) pointing to mirage-skeleton. Can be a local repository."
  in
  let default = "https://github.com/mirage/mirage-skeleton.git" in
  named
    (fun x -> `Mirage_skeleton x)
    Arg.(value & opt string default & info ~doc ~docv [ "mirage-skeleton" ])

let mirage =
  let open Cmdliner in
  let docv = "REPOSITORY" in
  let doc = "The $(docv) pointing to mirage. Can be a local repository." in
  let default = "https://github.com/mirage/mirage.git" in
  named
    (fun x -> `Mirage x)
    Arg.(value & opt string default & info ~doc ~docv [ "mirage" ])

let mirage_dev =
  let open Cmdliner in
  let docv = "REPOSITORY" in
  let doc = "The $(docv) pointing to mirage-dev. Can be a local repository." in
  let default = "https://github.com/mirage/mirage-dev.git" in
  named
    (fun x -> `Mirage_dev x)
    Arg.(value & opt string default & info ~doc ~docv [ "mirage-dev" ])

let main_ci =
  Mirage_ci_pipelines.PR.test_options_cmdliner
  |> named (fun x -> `Pipelines_options x)

let cmd : unit Current.or_error Term.t * Term.info =
  let doc = "Test the MirageOS CI pipeline locally" in
  ( Term.(
      const main
      $ Current.Config.cmdliner
      $ Current_web.cmdliner
      $ Common.Config.cmdliner
      $ main_ci
      $ opam_repository
      $ opam_overlays
      $ mirage_skeleton
      $ mirage_dev
      $ mirage),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
