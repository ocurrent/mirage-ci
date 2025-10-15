type ocaml_version = V4_14 | V5_3 | V5_4

let pp_ocaml f = function
  | V4_14 -> Fmt.pf f "4.14"
  | V5_3 -> Fmt.pf f "5.3"
  | V5_4 -> Fmt.pf f "5.4"

let pp_exact_ocaml f = function
  | V4_14 -> Fmt.pf f "4.14.2"
  | V5_3 -> Fmt.pf f "5.3.0"
  | V5_4 -> Fmt.pf f "5.4.0"

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

let platform_id t =
  match t.arch with
  | Arm64 -> "arm64-" ^ Fmt.str "%a" pp_system t.system
  | Amd64 -> "x86_64-" ^ Fmt.str "%a" pp_system t.system

let pp_platform f t =
  Fmt.pf f "%s-%a-%a" (arch_to_string t.arch) pp_os t.system.os pp_ocaml
    t.system.ocaml

let ocluster_pool { arch; _ } =
  match arch with Arm64 -> "linux-arm64" | Amd64 -> "linux-x86_64"

(* Base configuration.. *)

let platform_v414_amd64 =
  { system = { ocaml = V4_14; os = Debian }; arch = Amd64 }

let platform_v414_arm64 =
  { system = { ocaml = V4_14; os = Debian }; arch = Arm64 }

let platform_v53_amd64 =
  { system = { ocaml = V5_3; os = Debian }; arch = Amd64 }

let platform_v53_arm64 =
  { system = { ocaml = V5_3; os = Debian }; arch = Arm64 }

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
       | ("aarch64" | "arm64"), _ -> platform_v414_arm64
       | _ -> platform_v414_amd64)
  |> Option.value ~default:platform_v414_amd64
