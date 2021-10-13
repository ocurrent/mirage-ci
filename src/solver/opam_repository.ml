open Lwt.Infix
module Log = Solver_api.Solver.Log
module Store = Git_unix.Store

let open_store path =
  Git_unix.Store.v path >|= function
  | Ok x -> x
  | Error e ->
      Fmt.failwith "Failed to open opam-repository: %a" Store.pp_error e

let oldest_commit_with ~from clone_path pkgs =
  let from = Store.Hash.to_hex from in
  let paths =
    pkgs
    |> List.map (fun pkg ->
           let name = OpamPackage.name_to_string pkg in
           let version = OpamPackage.version_to_string pkg in
           Printf.sprintf "packages/%s/%s.%s" name name version)
  in
  let cmd =
    "git"
    :: "-C"
    :: Fpath.to_string clone_path
    :: "log"
    :: "-n"
    :: "1"
    :: "--format=format:%H"
    :: from
    :: "--"
    :: paths
  in
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim
