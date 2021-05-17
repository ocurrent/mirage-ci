type t

val cmdliner : t Cmdliner.Term.t

val v :
  ssh_host:string ->
  ?ssh_port:int ->
  ssh_repo:string ->
  http_remote:string ->
  private_key_file:string ->
  public_key_file:string ->
  t

module Cluster : sig
  val clone : branch:string -> directory:string -> t -> Obuilder_spec.op

  val push : ?force:bool -> t -> Obuilder_spec.op

  val secrets : t -> (string * string) list
end

val remote : t -> string

val http_remote : t -> string

val sync : job:Current.Job.t -> t -> unit Current.or_error Lwt.t

val with_clone :
  job:Current.Job.t ->
  branch:string ->
  t ->
  (Fpath.t -> 'a Current.or_error Lwt.t) ->
  'a Current.or_error Lwt.t

module type Reader = sig
  type t

  val pp : t Fmt.t

  val id : string

  val fn : Fpath.t -> t Lwt.t

  val marshal : t -> string

  val unmarshal : string -> t
end

val read :
  branch:string -> (module Reader with type t = 'a) -> t -> string Current.t -> 'a Current.t
