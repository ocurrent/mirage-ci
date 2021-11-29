val profile : [ `Dev | `Production | `Docker ]

type build_config

val cmdliner : build_config Cmdliner.Term.t

type t

val make : ?secrets:(string * string) list -> build_config -> t

val build :
  ?label:string ->
  ?cache_hint:string ->
  t ->
  pool:string ->
  src:Current_git.Commit_id.t Current.t list ->
  Spec.t Current.t ->
  unit Current.t
