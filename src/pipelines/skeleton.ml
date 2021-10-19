open Mirage_lib
open Current.Syntax
open Common

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let is_available_on (platform : Platform.t) = function
  | "unix" | "hvt" -> true
  | "xen" when platform.arch = Amd64 -> true
  | _ -> false

(* muen: no support for block *)
let overrides = [ ("block", targets |> List.filter (( <> ) "muen")) ]

type test = { platform : Platform.t; unikernel : string; target : string }

type configuration_main = {
  build : Mirage.mirage_builder;
  mirage : Current_git.Commit_id.t Current.t;
  repos : Repository.t list Current.t;
  skeleton : Current_git.Commit_id.t Current.t;
}

let run_test_mirage_main ~config { unikernel; platform; target } configuration =
  let c = configuration in
  let base =
    let+ repos = c.repos in
    Platform.spec platform.system |> Spec.add (Setup.add_repositories repos)
  in
  let base =
    let+ base = base and+ mirage = c.mirage in
    (* pre-install ocaml-freestanding *)
    Spec.add (Setup.install_tools [ "ocaml-freestanding" ]) base
    |> Spec.add
         [
           Obuilder_spec.run ~network:Setup.network "opam pin -n -y %s"
             (Setup.remote_uri mirage);
         ]
  in
  c.build ~config ~platform ~base ~project:c.skeleton ~unikernel ~target ()
  |> Current.collapse
       ~key:("Unikernel " ^ unikernel ^ "@" ^ target)
       ~value:("main-" ^ Platform.platform_id platform)
       ~input:c.repos

let test_stage ~stage ~unikernels ~target ~platform ~run_test configuration =
  unikernels
  |> List.filter (fun name ->
         overrides
         |> List.find_map (fun (n, t) -> if n = name then Some t else None)
         |> Option.map (List.mem target)
         |> Option.value ~default:true)
  |> List.map (fun name ->
         run_test
           { unikernel = stage ^ "/" ^ name; target; platform }
           configuration)
  |> Current.all

let multi_stage_test ~platform ~targets ~configure ~run_test mirage_skeleton =
  let* stages = Skeleton.stages mirage_skeleton in
  let rec aux ~target skeleton = function
    | [] -> skeleton |> Current.ignore_value
    | (name, stage, unikernels) :: q ->
        let configuration = configure skeleton in
        let test_stage =
          test_stage ~run_test ~stage ~unikernels ~target ~platform
            configuration
          |> Current.collapse ~key:("Test stage " ^ name) ~value:target
               ~input:skeleton
        in
        let mirage_skeleton =
          let+ _ = test_stage and+ skeleton = skeleton in
          skeleton
        in
        aux ~target mirage_skeleton q
  in
  targets
  |> List.filter (is_available_on platform)
  |> List.map (fun target -> (target, aux ~target mirage_skeleton stages))
  |> Current.all_labelled

(* MIRAGE MAIN TEST *)

let v ~build ~config ~platform ~mirage ~repos mirage_skeleton =
  let mirage_skeleton = Current_git.fetch mirage_skeleton in
  multi_stage_test ~platform ~targets
    ~run_test:(run_test_mirage_main ~config)
    ~configure:(fun skeleton ->
      let skeleton = Current.map Current_git.Commit.id skeleton in
      { mirage; repos; skeleton; build })
    mirage_skeleton

(* Test all of mirage-skeleton at once *)
let all_in_one_test ~(platform : Platform.t) ~target ~repos ~mirage_overlay
    ~config mirage_skeleton =
  let spec =
    let+ repos = repos and+ mirage_overlay = mirage_overlay in
    let open Obuilder_spec in
    Platform.spec platform.system
    |> Spec.add (Setup.add_repositories repos)
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

let all_in_one_test ~(platform : Platform.t) ~repos ~mirage_overlay ~config
    mirage_skeleton =
  targets
  |> List.filter (is_available_on platform)
  |> List.map (fun target ->
         ( target,
           all_in_one_test ~target ~platform ~repos ~mirage_overlay ~config
             mirage_skeleton ))
  |> Current.all_labelled
