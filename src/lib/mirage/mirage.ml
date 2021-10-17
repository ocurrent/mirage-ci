open Current.Syntax
module Setup = Common.Setup
module Spec = Common.Spec
module Config = Common.Config
module Platform = Common.Platform

type mirage_builder =
  config:Common.Config.t ->
  platform:Common.Platform.t ->
  base:Common.Spec.t Current.t ->
  project:Current_git.Commit_id.t Current.t ->
  unikernel:string ->
  target:string ->
  unit ->
  unit Current.t

let v_3 ~config ~(platform : Platform.t) ~base ~project ~unikernel ~target () =
  let spec =
    let+ base = base in
    let open Obuilder_spec in
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
  Config.build ~label ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src spec

let v_4 ~config ~(platform : Platform.t) ~base ~project ~unikernel ~target () =
  let spec =
    let+ base = base in
    let open Obuilder_spec in
    let mirage_configured =
      let switch_with_mirage =
        base |> Spec.add (Setup.install_tools [ "mirage" ]) |> Spec.finish
      in
      Platform.spec platform.system
      |> Spec.children ~name:"mirage-tool" switch_with_mirage
      |> Spec.add
           [
             copy [ "./" ^ unikernel ^ "/config.ml" ] ~dst:"/src/";
             copy ~from:(`Build "mirage-tool")
               [ "/home/opam/.opam/4.13" ]
               ~dst:"/home/opam/.opam/4.13";
             workdir "/src";
             run "sudo chown -R opam:opam .";
             run "opam exec -- mirage configure -t %s" target;
           ]
      |> Spec.finish
    in
    let mirage_depends =
      let switch_with_tools =
        let tools =
          match target with "unix" -> [] | _ -> [ "ocaml-freestanding" ]
        in
        base
        |> Spec.add (Setup.install_tools ("opam-monorepo" :: tools))
        |> Spec.finish
      in
      base
      |> Spec.children ~name:"mirage-configured" mirage_configured
      |> Spec.children ~name:"mirage-tools" switch_with_tools
      |> Spec.add
           [
             workdir "/src";
             copy ~from:(`Build "mirage-configured") [ "/src" ] ~dst:"/src";
             copy ~from:(`Build "mirage-tools")
               [ "/home/opam/.opam/4.13" ]
               ~dst:"/home/opam/.opam/4.13";
             env "DUNE_CACHE" "enabled";
             env "DUNE_CACHE_TRANSPORT" "direct";
             run
               ~cache:[ Setup.dune_build_cache; Setup.opam_download_cache ]
               ~network:[ "host" ]
               "opam exec -- make $(echo mirage/*-monorepo.opam).locked";
             (* opam-monorepo generates a lockfile, and tools are installed in the switch. *)
           ]
      |> Spec.finish
    in

    Platform.spec platform.system
    |> Spec.children ~name:"mirage-depends" mirage_depends
    |> Spec.add
         [
           workdir "/src";
           copy [ "./" ^ unikernel ] ~dst:"/src";
           copy ~from:(`Build "mirage-depends") [ "/src" ] ~dst:"/src";
           copy ~from:(`Build "mirage-depends")
             [ "/home/opam/.opam/4.13" ]
             ~dst:"/home/opam/.opam/4.13";
           run
             ~cache:[ Setup.opam_download_cache ]
             ~network:[ "host" ] "make depends";
           env "DUNE_CACHE" "enabled";
           env "DUNE_CACHE_TRANSPORT" "direct";
           run ~cache:[ Setup.dune_build_cache ] "opam exec -- dune build";
         ]
  in
  let label = unikernel ^ "@" ^ target in
  let src = [ project ] |> Current.list_seq in
  let cache_hint =
    Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
  in
  Config.build ~label ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src spec
