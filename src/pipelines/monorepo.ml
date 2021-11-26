module Git = Current_git
module Docker = Current_docker.Default
open Current.Syntax
open Monorepo_lib
open Common

type mode = UniverseEdge | MirageEdge | Released
type toolchain = Host | Freestanding

let pp_toolchain () = function Host -> "" | Freestanding -> "-x freestanding"

let get_monorepo_library ~mirage_only =
  let pp_libraries f Universe.{ name; sublibs; mirage } =
    if mirage || not mirage_only then
      let names =
        match sublibs with
        | [] -> [ name ]
        | lst -> List.map (fun sublibname -> name ^ "." ^ sublibname) lst
      in
      Fmt.pf f "%a" Fmt.(list ~sep:(const string " ") string) names
    else ()
  in
  let pp_project f (project : Universe.Project.t) =
    Fmt.pf f "@[%a @,@]"
      Fmt.(list ~sep:(const string " ") pp_libraries)
      project.opam
  in
  Fmt.str
    {|
  (library
   (name monorepo)
   (public_name monorepo)
   (libraries %a)
  )
  |}
    (Fmt.list pp_project)

let spec ~mode ~repos ~system ~toolchain ~lock =
  let open Obuilder_spec in
  let base =
    let+ repos = repos in
    Platform.spec system |> Spec.add (Setup.add_repositories repos)
  in
  let base =
    let+ base = base in
    match toolchain with
    | Host -> base
    | Freestanding ->
        Spec.add (Setup.install_tools [ "ocaml-freestanding" ]) base
  in
  let spec =
    match mode with
    | MirageEdge | Released -> Monorepo.spec ~base ~lock ()
    | UniverseEdge ->
        let+ base = base in
        base
        |> Spec.add [ workdir "/src"; run "sudo chown opam:opam /src" ]
        |> Spec.add (Setup.install_tools [ "dune" ])
  in
  match mode with
  | Released -> spec
  | MirageEdge ->
      let+ spec = spec in
      Spec.add
        [
          workdir "/src/duniverse";
          run "sudo chown opam:opam /src/duniverse";
          copy [ "." ] ~dst:"/src/duniverse/";
          workdir "/src";
        ]
        spec
  | UniverseEdge ->
      let+ spec = spec in
      Spec.add
        [
          workdir "/src/duniverse";
          run "sudo chown opam:opam /src/duniverse";
          copy [ "." ] ~dst:"/src/duniverse/";
          run "touch dune && mv dune dune_";
          run "echo '(vendored_dirs *)' >> dune";
          workdir "/src";
        ]
        spec

let v ~config ~(platform : Platform.t) ~roots ~mode ?(src = Current.return [])
    ?(toolchain = Host) ~repos ~lock () =
  let spec = spec ~system:platform.system ~mode ~repos ~toolchain ~lock in
  let mirage_only = match toolchain with Host -> false | _ -> true in
  let dune_build =
    let+ spec = spec in
    let open Obuilder_spec in
    Spec.add
      [
        run "echo '%s' >> dune" (get_monorepo_library ~mirage_only roots);
        run "touch monorepo.opam; touch monorepo.ml";
        (* Dune issue with strict_package_deps *)
        run
          "find . -type f -name 'dune-project' -exec sed \
           's/(strict_package_deps)//g' -i {} \\;";
        run
          "opam exec -- dune build --profile release --debug-dependency-path %a"
          pp_toolchain toolchain;
        run "du -sh _build/";
      ]
      spec
  in
  let name_of_toolchain =
    match toolchain with Host -> "host" | Freestanding -> "freestanding"
  in
  let name_of_mode =
    match mode with
    | UniverseEdge -> "universe-edge"
    | MirageEdge -> "mirage-edge"
    | Released -> "released"
  in
  let cache_hint =
    "mirage-ci-monorepo-" ^ Fmt.str "%a" Platform.pp_system platform.system
  in
  Config.build
    ~label:(name_of_toolchain ^ "-" ^ name_of_mode)
    ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src dune_build

let lock ~(system : Platform.system) ~value ~config ~store ~monorepo ~repos
    (projects : Universe.Project.t list) =
  Current.with_context repos (fun () ->
      let configuration =
        Monorepo.opam_file
          ~ocaml_version:(Fmt.str "%a" Platform.pp_exact_ocaml system.ocaml)
          projects
      in
      let key =
        Fmt.str "monorepo-%a-%s" Platform.pp_system system
          (Opamfile.digest configuration)
      in
      Monorepo.lock ~key ~value ~config ~store ~repos ~system
        ~opam:(Current.return configuration)
        monorepo)

let universe_edge ~config ~platform ~git_store ~roots ~repos ~lock =
  let src =
    let+ src =
      Monorepo_git_push.v git_store ~branch:"universe-edge-monorepo"
        (Monorepo_lock.commits lock)
    in
    [ src ]
  in
  [
    ( "universe-edge-freestanding",
      v ~config ~platform ~src ~roots ~mode:UniverseEdge ~toolchain:Freestanding
        ~repos ~lock () );
    ( "universe-edge-host",
      v ~config ~platform ~src ~roots ~mode:UniverseEdge ~repos ~lock () );
  ]
  |> Current.all_labelled

let mirage_edge ~config ~platform ~git_store ~roots ~repos ~lock =
  let filter (project : Monorepo_lock.project) =
    List.exists
      (fun (prj : Universe.Project.t) ->
        Astring.String.find_sub ~sub:prj.repo project.repo |> Option.is_some)
      roots
  in
  let src =
    let+ src =
      Monorepo_git_push.v git_store ~branch:"mirage-edge-monorepo"
        (Monorepo_lock.commits ~filter lock)
    in
    [ src ]
  in
  [
    ( "mirage-edge-freestanding",
      v ~config ~platform ~src ~roots ~mode:MirageEdge ~toolchain:Freestanding
        ~repos ~lock () );
    ( "mirage-edge-host",
      v ~config ~platform ~src ~roots ~mode:MirageEdge ~repos ~lock () );
  ]
  |> Current.all_labelled

let released ~config ~platform ~roots ~repos ~lock =
  [
    ( "released-freestanding",
      v ~config ~platform ~roots ~mode:Released ~toolchain:Freestanding ~repos
        ~lock () );
    ("released-host", v ~config ~platform ~roots ~mode:Released ~repos ~lock ());
  ]
  |> Current.all_labelled
