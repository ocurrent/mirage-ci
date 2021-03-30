type solve_result = { packages : string list; commits : (string * string) list } [@@deriving yojson]

val main : unit -> unit
(** [main hash] runs a worker process that reads requests from stdin and writes results to stdout,
    using commit [hash] in opam-repository. *)
