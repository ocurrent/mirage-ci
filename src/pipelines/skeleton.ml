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
  repos : Repository.t list Current.t;
  skeleton : Current_git.Commit_id.t Current.t;
}

let run_test_mirage_main ~config { unikernel; platform; target } configuration =
  let c = configuration in
  let base =
    let+ repos = c.repos in
    Platform.spec platform.system |> Spec.add (Setup.add_repositories repos)
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
  let n_stages = List.length stages in
  let rec aux ~target skeleton = function
    | [] -> skeleton |> Current.ignore_value
    | (name, stage, unikernels) :: q ->
        let configuration = configure skeleton in
        let key = Fmt.str "Test stage %s (%d)" name n_stages in
        let test_stage =
          test_stage ~run_test ~stage ~unikernels ~target ~platform
            configuration
          |> Current.collapse ~key ~value:target ~input:skeleton
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

let v ~build ~config ~platform ~repos mirage_skeleton =
  let mirage_skeleton = Current_git.fetch mirage_skeleton in
  multi_stage_test ~platform ~targets
    ~run_test:(run_test_mirage_main ~config)
    ~configure:(fun skeleton ->
      let skeleton = Current.map Current_git.Commit.id skeleton in
      { repos; skeleton; build })
    mirage_skeleton

type repo = { name : string; branch : string }

type test_set = {
  name : string;
  mirage_skeleton : repo;
  mirage_dev : repo;
  build : Mirage_lib.Mirage.mirage_builder;
}

type test_options = { mirage_4 : bool; mirage_3 : bool }

let test_options_cmdliner =
  let open Cmdliner in
  let mirage_4 = Arg.(value & flag & info [ "test-mirage-4" ]) in
  let mirage_3 = Arg.(value & flag & info [ "test-mirage-3" ]) in
  let make mirage_3 mirage_4 = { mirage_3; mirage_4 } in
  Term.(const make $ mirage_3 $ mirage_4)

let tests options =
  let m4 =
    if options.mirage_4 then
      [
        {
          name = "mirage-4";
          mirage_dev = { name = "mirage-dev"; branch = "master" };
          mirage_skeleton = { name = "mirage-skeleton"; branch = "mirage-dev" };
          build = Mirage_lib.Mirage.v_4;
        };
      ]
    else []
  in
  let m3 =
    if options.mirage_3 then
      [
        {
          name = "mirage-3";
          mirage_dev = { name = "mirage-dev"; branch = "3" };
          mirage_skeleton = { name = "mirage-skeleton"; branch = "master" };
          build = Mirage_lib.Mirage.v_3;
        };
      ]
    else []
  in
  m3 @ m4

let local ~config ~options repos =
  let setup { branch; name } = (name, branch) in
  let pipelines =
    tests options
    |> List.map (fun { name; mirage_dev; mirage_skeleton; build; _ } ->
           let grefs = List.map setup [ mirage_dev; mirage_skeleton ] in
           let repos = repos grefs in
           let find_repo (m : repo) = Current.map (List.assoc m.name) repos in
           let mirage_skeleton = find_repo mirage_skeleton in
           ( name,
             v ~build ~config ~platform:Common.Platform.platform_host ~repos
               mirage_skeleton ))
  in
  Current.all_labelled pipelines
