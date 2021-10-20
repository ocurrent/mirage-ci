open Current.Syntax
open Common

type 'a build_mode = Mirage_3 | Mirage_4 of { overlay : 'a option }

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let is_available_on (platform : Platform.t) = function
  | "unix" | "hvt" -> true
  | "xen" when platform.arch = Amd64 -> true
  | _ -> false

let add_make_instructions build_mode spec =
  let open Obuilder_spec in
  match build_mode with
  | Mirage_3 ->
      let+ spec = spec in
      Spec.add
        [
          run "opam exec -- make configure";
          env "DUNE_CACHE" "enabled";
          env "DUNE_CACHE_TRANSPORT" "direct";
          run
            ~cache:[ Setup.opam_download_cache; Setup.dune_build_cache ]
            ~network:[ "host" ] "opam exec -- make build";
        ]
        spec
  | Mirage_4 { overlay } ->
      let spec =
        Current.map (Spec.add [ run "opam exec -- make configure" ]) spec
      in
      let+ spec =
        match overlay with
        | None -> spec
        | Some o ->
            let+ o = o and+ spec = spec in
            let name = "merge-repo-add-overlay" in
            let target_name = "/merged-repositories/" ^ name in
            spec
            |> Spec.children ~name:"merge-repo-add-overlay"
                 (Merge_commit.spec o)
            |> Spec.add
                 [
                   copy ~from:(`Build name) [ "/merged" ] ~dst:target_name;
                   env "OVERLAY" target_name;
                 ]
      in
      Spec.add
        [
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
        spec

(* Test all of mirage-skeleton at once *)
let all_in_one_test ~(platform : Platform.t) ~target ~repos ~mirage ~config
    ~build_mode mirage_skeleton =
  let spec_preparation =
    (* Set-up repositories and install the mirage tool *)
    let+ repos = repos and+ mirage = mirage in
    let open Obuilder_spec in
    let pin_mirage base =
      match mirage with
      | Some merged_commit ->
          let name = "merge-repo-pin-mirage" in
          let target_name = "/merged-repositories/" ^ name in
          Spec.children ~name (Merge_commit.spec merged_commit) base
          |> Spec.add
               [
                 copy ~from:(`Build name) [ "/merged" ] ~dst:target_name;
                 run ~network:[ "host" ] "opam pin -ny %s" target_name;
               ]
      | None -> base
    in
    let merge_repos base =
      List.fold_left
        (fun (base, i) repo ->
          let name = "merge-repo-" ^ string_of_int i in
          let target_name = "/merged-repositories/" ^ name in
          let with_child_build =
            Spec.children ~name (Merge_commit.spec repo) base
          in
          let with_repo =
            with_child_build
            |> Spec.add
                 [
                   copy ~from:(`Build name) [ "/merged" ] ~dst:target_name;
                   run ~network:[ "host" ] "opam repo add %s %s" name
                     target_name;
                 ]
          in
          (with_repo, i + 1))
        (base, 0) repos
      |> fst
    in
    Platform.spec platform.system
    |> merge_repos
    |> pin_mirage
    |> Spec.add (Setup.install_tools [ "mirage" ])
    |> Spec.add
         [ copy [ "." ] ~dst:"/src/"; env "MODE" target; workdir "/src/" ]
  in
  (* Perform the build, the exact behavior depending on the build mode *)
  let spec = add_make_instructions build_mode spec_preparation in

  let label = "skeleton@" ^ target in
  let cache_hint =
    Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
  in
  let src = Current.map Merge_commit.to_list mirage_skeleton in
  Config.build ~label ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src spec

let all_in_one_test ~(platform : Platform.t) ~repos ~mirage ~config ~build_mode
    mirage_skeleton =
  targets
  |> List.filter (is_available_on platform)
  |> List.map (fun target ->
         ( target,
           all_in_one_test ~target ~platform ~repos ~mirage ~config ~build_mode
             mirage_skeleton ))
  |> Current.all_labelled
