type resolution = { name : string; version : string } [@@deriving yojson]
type t = { resolutions : resolution list; repos : Repository.t list }

let solver = Solver_pool.spawn_local ()

let job_log job =
  let module X = Solver_api.Raw.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

module Op = struct
  type t = No_context

  module Key = struct
    type t = {
      repos : (string * Current_git.Commit.t) list;
      packages : string list;
      system : Platform.system;
    }

    let digest { repos; packages; system } =
      let json =
        `Assoc
          [
            ( "repos",
              `List
                (List.map
                   (fun (_, commit) -> `String (Current_git.Commit.hash commit))
                   repos) );
            ("packages", `List (List.map (fun p -> `String p) packages));
            ("system", `String (Fmt.str "%a" Platform.pp_system system));
          ]
      in
      Yojson.to_string json
  end

  module Value = struct
    type t = { resolutions : resolution list; repos : (string * string) list }
    [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let auto_cancel = true
  let id = "mirage-ci-solver"
  let pp f _ = Fmt.string f "Opam solver"

  open Lwt.Syntax

  let with_checkouts ~job commits fn =
    let rec aux acc = function
      | [] -> fn (List.rev acc)
      | commit :: next ->
          Current_git.with_checkout ~job commit (fun tmpdir ->
              aux (tmpdir :: acc) next)
    in
    aux [] commits

  let build No_context job { Key.repos; packages; system } =
    let* () = Current.Job.start ~level:Harmless job in
    let repos_git = List.map snd repos in
    with_checkouts ~job repos_git @@ fun dirs ->
    let constraints =
      [ ("ocaml", Fmt.to_to_string Platform.pp_exact_ocaml system.ocaml) ]
    in
    let opam_repos_folders =
      List.combine dirs repos
      |> List.map (fun (dir, (name, repo)) ->
             (name, Fpath.to_string dir, Current_git.Commit.hash repo))
    in
    let pkgs = "ocaml" :: packages in
    let request =
      Solver_api.Worker.Solve_request.
        {
          opam_repos_folders;
          pkgs;
          constraints;
          platforms =
            [
              ( "default",
                Solver_api.Worker.Vars.
                  {
                    arch = "x86_64";
                    os = "linux";
                    os_distribution = "linux";
                    os_family = Platform.os_family system.os;
                    os_version = Platform.os_version system.os;
                  } );
            ];
        }
    in
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    let+ r = Solver_api.Solver.solve solver request ~log in

    match r with
    | Ok [] -> Fmt.error_msg "no platform"
    | Ok [ { commits; packages; _ } ] ->
        let resolutions =
          List.map
            (fun pkg ->
              let opam = OpamPackage.of_string pkg in
              {
                name = OpamPackage.name_to_string opam;
                version = OpamPackage.version_to_string opam;
              })
            packages
        in
        Ok Value.{ resolutions; repos = commits }
    | Ok _ -> Fmt.error_msg "??"
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg
end

module Solver_cache = Current_cache.Make (Op)
module Git = Current_git

let v ~system ~repos ~packages =
  let open Current.Syntax in
  Current.component "solver (%s)" (String.concat "," packages)
  |> let> repos = repos in
     Solver_cache.get No_context { system; repos; packages }
     |> Current.Primitive.map_result
          (Result.map (fun Op.Value.{ resolutions; repos = repos_raw } ->
               let repos =
                 repos_raw
                 |> List.map (fun (name, hash) ->
                        let commit = List.assoc name repos in
                        let id = Git.Commit.id commit in
                        ( name,
                          Git.Commit_id.v ~repo:(Git.Commit_id.repo id)
                            ~gref:(Git.Commit_id.gref id) ~hash ))
               in
               { resolutions; repos }))
