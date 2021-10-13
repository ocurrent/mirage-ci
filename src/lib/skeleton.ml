module Git = Current_git

type stage = string * string * string list [@@deriving yojson]
type stages = stage list [@@deriving yojson]

let pool = Current.Pool.create ~label:"skeleton-stages" 8

let stages_spec =
  Fpath.
    [
      ("1: test-target", v "tutorial", Some [ "noop" ]);
      ( "2: tutorial",
        v "tutorial",
        Some [ "noop-functor"; "hello"; "hello-key"; "app_info" ] );
      ("3: tutorial-lwt", v "tutorial" / "lwt", None);
      ("4: devices", v "device-usage", None);
      ("5: applications", v "applications", None);
    ]

let blacklist = [ "tracing" ] (* not supported at all *)

let test_unikernel path =
  let unikernel_name = Fpath.basename path in
  if
    (not (List.mem unikernel_name blacklist))
    && Bos.OS.Path.exists Fpath.(path / "config.ml")
       |> Result.value ~default:false
  then Some unikernel_name
  else None

let do_stage ~path (name, root, override) =
  match override with
  | Some unikernels -> (name, Fpath.to_string root, unikernels)
  | None ->
      let folders = Bos.OS.Dir.contents Fpath.(path // root) |> Result.get_ok in
      let unikernels = List.filter_map test_unikernel folders in
      (name, Fpath.to_string root, unikernels)

module SkeletonStages = struct
  type t = No_context

  module Key = struct
    type t = Git.Commit.t

    let digest = Git.Commit.hash
  end

  module Value = struct
    type t = stages [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let pp f _ = Fmt.pf f "Skeleton stages"
  let id = "skeleton-stages"
  let auto_cancel = true

  let build No_context job commit =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~pool ~job commit @@ fun path ->
    let spec = List.map (do_stage ~path) stages_spec in
    List.iter
      (fun (name, root, unikernels) ->
        Current.Job.log job "%s: %s -> %a" name root
          Fmt.(list ~sep:(any ", ") string)
          unikernels)
      spec;
    Lwt.return_ok spec
end

module SkeletonStagesCache = Current_cache.Make (SkeletonStages)

let stages t =
  let open Current.Syntax in
  Current.component "skeleton stages"
  |> let> t = t in
     SkeletonStagesCache.get No_context t
