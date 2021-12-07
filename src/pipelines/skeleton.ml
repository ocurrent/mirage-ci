open Current.Syntax
open Common

type 'a build_mode = Mirage_3 | Mirage_4 of { overlay : 'a option }

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let is_available_on (platform : Platform.t) = function
  | "unix" | "hvt" -> true
  | "xen" when platform.arch = Amd64 -> true
  | _ -> false

let make_instructions =
  let open Obuilder_spec in
  function
  | Mirage_3 ->
      Current.return
        [
          run "opam exec -- make configure";
          env "DUNE_CACHE" "enabled";
          env "DUNE_CACHE_TRANSPORT" "direct";
          run
            ~cache:[ Setup.opam_download_cache; Setup.dune_build_cache ]
            ~network:[ "host" ] "opam exec -- make build";
        ]
  | Mirage_4 { overlay } ->
      let+ overlay =
        match overlay with
        | None -> Current.return []
        | Some o ->
            let+ o = o in
            [ env "OVERLAY" (Setup.remote_uri o) ]
      in

      [ run "opam exec -- make configure" ]
      @ overlay
      @ [
          run ~network:[ "host" ] "opam exec -- make lock";
          run
            ~cache:[ Setup.opam_download_cache ]
            ~network:[ "host" ] "opam exec -- make depends";
          run "opam monorepo list -l mirage.opam.locked";
          run
            ~cache:[ Setup.opam_download_cache ]
            ~network:[ "host" ] "opam exec -- make pull";
          env "DUNE_CACHE" "enabled";
          env "DUNE_CACHE_TRANSPORT" "direct";
          run ~cache:[ Setup.dune_build_cache ] "opam exec -- make build";
        ]

(* Test all of mirage-skeleton at once *)
let all_in_one_test ~(platform : Platform.t) ~target ~repos ~mirage ~config
    ~build_mode mirage_skeleton =
  let spec =
    let+ repos = repos
    and+ make_instructions = make_instructions build_mode
    and+ mirage = mirage in

    let open Obuilder_spec in
    let pin_mirage =
      match mirage with
      | Some commit ->
          [
            run ~network:[ "host" ] "opam pin -ny %s" (Setup.remote_uri commit);
          ]
      | None -> []
    in
    Platform.spec platform.system
    |> Spec.add (Setup.add_repositories repos)
    |> Spec.add pin_mirage
    |> Spec.add (Setup.install_tools [ "mirage" ])
    |> Spec.add
         [ copy [ "." ] ~dst:"/src/"; env "MODE" target; workdir "/src/" ]
    |> Spec.add make_instructions
  in
  let label = "skeleton@" ^ target in
  let cache_hint =
    Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system
  in
  let src = [ mirage_skeleton ] in
  Config.build ~label ~cache_hint config
    ~pool:(Platform.ocluster_pool platform)
    ~src spec
  |> Current_web_pipelines.Task.single label

let all_in_one_test ~(platform : Platform.t) ~repos ~mirage ~config ~build_mode
    mirage_skeleton =
  targets
  |> List.filter (is_available_on platform)
  |> List.map (fun target ->
         all_in_one_test ~target ~platform ~repos ~mirage ~config ~build_mode
           mirage_skeleton)
  |> Current_web_pipelines.Task.all
  |> Current_web_pipelines.Task.map_state (fun jobs ->
         {
           Current_web_pipelines.State.jobs;
           metadata = Fmt.to_to_string Platform.pp_platform platform;
         })
