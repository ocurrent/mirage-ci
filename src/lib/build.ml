open Current.Syntax
open Obuilder_spec

module Mirage_3 = struct
  let run ~ocluster ~(platform : Platform.t) ~base ~project ~unikernel ~target
      () =
    let spec =
      let+ base = base in
      base
      |> Spec.add (Setup.install_tools [ "dune"; "mirage"; "opam-monorepo" ])
      |> Spec.add
           [
             copy
               [ "./" ^ unikernel ^ "/config.ml" ]
               ~dst:("/src/" ^ unikernel ^ "/");
             workdir ("/src/" ^ unikernel);
             run "sudo chown -R opam:opam .";
             env "DUNE_CACHE" "enabled";
             run ~cache:[ Setup.dune_build_cache ]
               "opam exec -- mirage configure -t %s" target;
             run
               ~cache:[ Setup.opam_download_cache; Setup.dune_build_cache ]
               ~network:Setup.network "opam exec -- make depends";
             copy [ "./" ^ unikernel ^ "/" ] ~dst:("/src/" ^ unikernel);
             run ~cache:[ Setup.dune_build_cache ] "opam exec -- mirage build";
           ]
    in
    let label = unikernel ^ "@" ^ target in
    let src = [ project ] |> Current.list_seq in
    let cache_hint =
      Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
    in
    Config.build ~label ~cache_hint ocluster
      ~pool:(Platform.ocluster_pool platform)
      ~src spec
end

module Mirage_4 = struct
  type stage = Configure | Depends | Lock | Pull | Build | Clean
  [@@deriving yojson]

  let name = function
    | Configure -> "configure"
    | Depends -> "depends"
    | Lock -> "lock"
    | Pull -> "pull"
    | Build -> "build"
    | Clean -> "clean"

  let add_make ?cache ?network s =
    Spec.add [ (run ?cache ?network "opam exec -- make %s") s ]

  let run ~ocluster ~(platform : Platform.t) ~base ~project ~target stage =
    let spec =
      let+ base = base in
      let base =
        base
        |> Spec.add
             [ run "sudo chown -R opam:opam ."; env "DUNE_CACHE" "enabled" ]
        |> Spec.add (Setup.install_tools [ "dune"; "mirage"; "opam-monorepo" ])
      in
      let configure =
        base
        |> add_make ~cache:[ Setup.dune_build_cache ]
             (Fmt.str "configure MODE=%s" target)
      in
      let depends =
        configure
        |> add_make
             ~cache:[ Setup.opam_download_cache; Setup.dune_build_cache ]
             ~network:Setup.network "depends"
      in
      let lock = depends |> add_make "lock" in
      let pull =
        lock
        |> add_make
             ~cache:[ Setup.opam_download_cache ]
             ~network:Setup.network "lock"
      in
      let build = pull |> add_make ~cache:[ Setup.dune_build_cache ] "build" in
      let clean = build |> add_make "clean" in
      match stage with
      | Configure -> configure
      | Depends -> depends
      | Lock -> lock
      | Pull -> pull
      | Build -> build
      | Clean -> clean
    in
    let label = "make " ^ name stage ^ "@" ^ target in
    let src = [ project ] |> Current.list_seq in
    let cache_hint =
      Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
    in
    Config.build ~label ~cache_hint ocluster
      ~pool:(Platform.ocluster_pool platform)
      ~src spec
end
