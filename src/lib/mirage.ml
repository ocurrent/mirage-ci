open Current.Syntax

let build ~ocluster ~(platform : Platform.t) ~base ~project ~unikernel ~target
    () =
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
  Config.build ~label ~cache_hint ocluster
    ~pool:(Platform.ocluster_pool platform)
    ~src spec
