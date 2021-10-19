open Current.Syntax
open Common

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let is_available_on (platform : Platform.t) = function
  | "unix" | "hvt" -> true
  | "xen" when platform.arch = Amd64 -> true
  | _ -> false

(* Test all of mirage-skeleton at once *)
let all_in_one_test ~(platform : Platform.t) ~target ~repos ~mirage
    ~mirage_overlay ~config mirage_skeleton =
  let spec =
    let+ repos = repos
    and+ mirage_overlay = mirage_overlay
    and+ mirage = mirage in

    let open Obuilder_spec in
    let pin_mirage =
      match mirage with
      | Some commit -> [ run "opam pin -ny %s" (Setup.remote_uri commit) ]
      | None -> []
    in
    Platform.spec platform.system
    |> Spec.add (Setup.add_repositories repos)
    |> Spec.add pin_mirage
    |> Spec.add (Setup.install_tools [ "mirage" ])
    |> Spec.add
         [
           copy [ "." ] ~dst:"/src/";
           env "MODE" target;
           workdir "/src/";
           run "opam exec -- make configure";
           env "OVERLAY" (Setup.remote_uri mirage_overlay);
           run ~network:[ "host" ] "opam exec -- make lock";
           run
             ~cache:[ Setup.opam_download_cache ]
             ~network:[ "host" ] "opam exec -- make depends";
           run
             ~cache:[ Setup.opam_download_cache ]
             ~network:[ "host" ] "opam exec -- make pull";
           env "DUNE_CACHE" "enabled";
           env "DUNE_CACHE_TRANSPORT" "direct";
           run ~cache:[ Setup.dune_build_cache ] "opam exec -- make build";
         ]
  in
  let label = "skeleton@" ^ target in
  let cache_hint =
    Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
  in
  let src =
    let+ mirage_skeleton = mirage_skeleton in
    [ mirage_skeleton ]
  in
  Config.build ~label ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src spec

let all_in_one_test ~(platform : Platform.t) ~repos ~mirage ~mirage_overlay
    ~config mirage_skeleton =
  targets
  |> List.filter (is_available_on platform)
  |> List.map (fun target ->
         ( target,
           all_in_one_test ~target ~platform ~repos ~mirage ~mirage_overlay
             ~config mirage_skeleton ))
  |> Current.all_labelled
