open Mirage_ci_lib
open Current.Syntax

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let is_available_on (platform : Platform.t) = function
  | "unix" | "hvt" -> true
  | "xen" when platform.arch = Amd64 -> true
  | _ -> false

(* muen: no support for block *)
let overrides = [ ("block", targets |> List.filter (( <> ) "muen")) ]

type configuration_4 = {
  repos : Repository.fetched list Current.t;
  skeleton : Current_git.Commit.t Current.t;
}

type test = { platform : Platform.t; unikernel : string; target : string }

let run_test_mirage_4 ~ocluster { unikernel; platform; target } configuration =
  let c = configuration in
  let repos =
    let+ repos = c.repos in
    List.map Repository.unfetch repos
  in
  let base =
    let+ repos = repos in
    Platform.spec platform.system |> Spec.add (Setup.add_repositories repos)
  in
  let base =
    let+ base = base in
    (* pre-install ocaml-freestanding *)
    Spec.add (Setup.install_tools [ "ocaml-freestanding" ]) base
  in
  let skeleton =
    (* add a fake dep to the lockfile (only rebuild if lockfile changed.)*)
    let+ skeleton = c.skeleton in
    Current_git.Commit.id skeleton
  in
  Mirage.build ~ocluster ~platform ~base ~project:skeleton ~unikernel ~target ()
  |> Current.collapse
       ~key:("Unikernel " ^ unikernel ^ "@" ^ target)
       ~value:("4-" ^ Platform.platform_id platform)
       ~input:c.repos

type configuration_main = {
  mirage : Current_git.Commit_id.t Current.t;
  repos : Repository.t list Current.t;
  skeleton : Current_git.Commit_id.t Current.t;
}

let run_test_mirage_main ~ocluster { unikernel; platform; target } configuration =
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
         [ Obuilder_spec.run ~network:Setup.network "opam pin -n -y %s" (Setup.remote_uri mirage) ]
  in
  Mirage.build ~ocluster ~platform ~base ~project:c.skeleton ~unikernel ~target ()
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
         run_test { unikernel = stage ^ "/" ^ name; target; platform } configuration)
  |> Current.all

let multi_stage_test ~platform ~targets ~configure ~run_test mirage_skeleton =
  let* stages = Skeleton.stages mirage_skeleton in
  let rec aux ~target skeleton = function
    | [] -> skeleton |> Current.ignore_value
    | (name, stage, unikernels) :: q ->
        let configuration = configure skeleton in
        let test_stage =
          test_stage ~run_test ~stage ~unikernels ~target ~platform configuration
          |> Current.collapse ~key:("Test stage " ^ name) ~value:target ~input:skeleton
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

(* MIRAGE 4 TEST *)

let v_4 ~ocluster ~repos ~(platform : Platform.t) ~targets mirage_skeleton =
  multi_stage_test ~platform ~targets ~run_test:(run_test_mirage_4 ~ocluster)
    ~configure:(fun skeleton -> { repos; skeleton })
    mirage_skeleton

(* MIRAGE MAIN TEST *)

let v_main ~ocluster ~platform ~mirage ~repos mirage_skeleton =
  let mirage_skeleton = Current_git.fetch mirage_skeleton in
  multi_stage_test ~platform ~targets ~run_test:(run_test_mirage_main ~ocluster)
    ~configure:(fun skeleton ->
      let skeleton = Current.map Current_git.Commit.id skeleton in
      { mirage; repos; skeleton })
    mirage_skeleton
