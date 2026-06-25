type ocaml_version = V4_14 | V5_4

let pp_ocaml f = function V4_14 -> Fmt.pf f "4.14" | V5_4 -> Fmt.pf f "5.4"

let pp_exact_ocaml f = function
  | V4_14 -> Fmt.pf f "4.14.4"
  | V5_4 -> Fmt.pf f "5.4.1"

type os = Debian | Ubuntu | Fedora

let os_version = function Ubuntu -> "24.04" | Fedora -> "42" | Debian -> "13"

let os_family = function
  | Ubuntu -> "ubuntu"
  | Fedora -> "fedora"
  | Debian -> "debian"

let pp_os f t = Fmt.pf f "%s-%s" (os_family t) (os_version t)

type arch = Arm64 | Amd64

let arch_to_string = function Arm64 -> "arm64" | Amd64 -> "x86_64"

type system = { ocaml : ocaml_version; os : os }

let pp_system f { ocaml; os } = Fmt.pf f "%a-ocaml-%a" pp_os os pp_ocaml ocaml
let spec t = Spec.make @@ Fmt.str "ocaml/opam:%a" pp_system t

type t = { system : system; arch : arch }

let docker_arch = function Arm64 -> "arm64" | Amd64 -> "amd64"

let platform_id t =
  match t.arch with
  | Arm64 -> "arm64-" ^ Fmt.str "%a" pp_system t.system
  | Amd64 -> "x86_64-" ^ Fmt.str "%a" pp_system t.system

let pp_platform f t =
  Fmt.pf f "%s-%a-%a" (arch_to_string t.arch) pp_os t.system.os pp_ocaml
    t.system.ocaml

let ocluster_pool { arch; _ } =
  match arch with Arm64 -> "linux-arm64" | Amd64 -> "linux-x86_64"

(* The [ocaml/opam:<distro>-ocaml-<v>] tag is a floating multi-arch tag that
   upstream rebuilds (e.g. when a new patch release of OCaml lands). obuilder
   caches a fetched base image by the literal [from] string, so referencing the
   floating tag directly means a worker never picks up a rebuild. Instead we
   periodically re-resolve the tag to a concrete per-arch digest via
   [docker manifest inspect] and feed that into the spec: when the digest moves,
   obuilder's cache key changes and the worker re-fetches. Mirrors ocaml-ci,
   including its 30-day cadence: upstream only rebuilds these base images
   occasionally, so re-resolving more often just burns build cycles. *)
let base_image_schedule =
  Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) ()

let pull_base t =
  let open Current.Syntax in
  let tag = Fmt.str "ocaml/opam:%a" pp_system t.system in
  Current.component "pull@,%a" pp_platform t
  |> let> () = Current.return () in
     Current_docker.Raw.peek ~docker_context:None
       ~schedule:base_image_schedule ~arch:(docker_arch t.arch) tag

(* Base configuration.. *)

let platform_v414_amd64 =
  { system = { ocaml = V4_14; os = Debian }; arch = Amd64 }

let platform_v414_arm64 =
  { system = { ocaml = V4_14; os = Debian }; arch = Arm64 }

let platform_v54_amd64 =
  { system = { ocaml = V5_4; os = Debian }; arch = Amd64 }

let platform_v54_arm64 =
  { system = { ocaml = V5_4; os = Debian }; arch = Arm64 }

let platform_host =
  Bos.Cmd.(v "uname" % "-m")
  |> Bos.OS.Cmd.run_out
  |> Bos.OS.Cmd.out_string
  |> Result.to_option
  |> Option.map (function
       | ("aarch64" | "arm64"), _ -> platform_v54_arm64
       | _ -> platform_v54_amd64)
  |> Option.value ~default:platform_v54_amd64
