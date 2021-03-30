module Worker = Solver_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store
open Lwt.Infix
open Lwt.Syntax

let env (vars : Worker.Vars.t) =
  Opam_0install.Dir_context.std_env ~arch:vars.arch ~os:vars.os
    ~os_distribution:vars.os_distribution ~os_version:vars.os_version ~os_family:vars.os_family ()

let solve ~packages ~constraints ~root_pkgs (vars : Worker.Vars.t) =
  let context = Git_context.create () ~packages ~env:(env vars) ~constraints in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context root_pkgs in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels -> Ok (Solver.packages_of_result sels)
  | Error diagnostics -> Error (Solver.diagnostics diagnostics)

type solve_result = { packages : string list; commits : (string * string) list } [@@deriving yojson]

let find_oldest_commits ~all (packages : OpamPackage.t list) =
  let module StringMap = Map.Make (String) in
  let store_map = ref StringMap.empty in
  List.iter
    (fun package ->
      let open OpamPackage in
      let (name, folder, commit), _ =
        all |> Name.Map.find (name package) |> Version.Map.find (version package)
      in
      match StringMap.find_opt folder !store_map with
      | None -> store_map := StringMap.add folder (name, commit, [ package ]) !store_map
      | Some (_, _, v) -> store_map := StringMap.add folder (name, commit, package :: v) !store_map)
    packages;
  !store_map |> StringMap.bindings
  |> Lwt_list.map_p (fun (folder, (name, commit, packages)) ->
         Opam_repository.oldest_commit_with ~from:(Store.Hash.of_hex commit) (Fpath.v folder)
           packages
         >|= fun commit -> (name, commit))

let main () =
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> Lwt.return_unit
    | len ->
        let len = int_of_string len in
        let data = really_input_string stdin len in
        let request =
          Worker.Solve_request.of_yojson (Yojson.Safe.from_string data) |> Result.get_ok
        in
        let { Worker.Solve_request.opam_repos_folders; pkgs; constraints; platforms } = request in
        Lwt_list.map_p
          (fun (name, folder, commit) ->
            Opam_repository.open_store (Fpath.v folder) >>= fun store ->
            Git_context.read_packages store (Store.Hash.of_hex commit) >|= fun res ->
            ((name, folder, commit), res))
          opam_repos_folders
        >>= fun packages ->
        let all_packages =
          (* TODO: check the priority rules *)
          List.fold_left
            (fun acc (repo_name, packages) ->
              packages
              |> OpamPackage.Name.Map.map (OpamPackage.Version.Map.map (fun x -> (repo_name, x)))
              |> OpamPackage.Name.Map.union (OpamPackage.Version.Map.union (fun a _ -> a)) acc)
            OpamPackage.Name.Map.empty packages
        in
        let root_pkgs = pkgs |> List.map OpamPackage.Name.of_string in
        let constraints =
          constraints
          |> List.map (fun (name, version) ->
                 (OpamPackage.Name.of_string name, (`Eq, OpamPackage.Version.of_string version)))
          |> OpamPackage.Name.Map.of_list
        in
        let* () =
          platforms
          |> Lwt_list.iter_s (fun (_id, platform) ->
                 let+ msg =
                   match
                     solve
                       ~packages:(OpamPackage.(Name.Map.map (Version.Map.map snd)) all_packages)
                       ~constraints ~root_pkgs platform
                   with
                   | Ok packages ->
                       let+ commits = find_oldest_commits ~all:all_packages packages in
                       let packages = List.map OpamPackage.to_string packages in
                       "+" ^ (solve_result_to_yojson { packages; commits } |> Yojson.Safe.to_string)
                   | Error msg -> Lwt.return ("-" ^ msg)
                 in
                 Printf.printf "%d\n%s%!" (String.length msg) msg)
        in
        aux ()
  in
  Lwt_main.run (aux ())

let main () =
  try main ()
  with ex ->
    Fmt.epr "solver bug: %a@." Fmt.exn ex;
    let msg = match ex with Failure msg -> msg | ex -> Printexc.to_string ex in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex
