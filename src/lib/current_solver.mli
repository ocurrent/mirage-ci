type resolution = { name : string; version : string }

type t = {
  resolutions: resolution list;
  repos: Repository.t list;
}

val v :
  system:Platform.system ->
  repos:Repository.fetched list Current.t ->
  packages:string list ->
  t Current.t
(** [v ~system ~repos ~packages] resolves the requested [packages] using the 
  given [repos] on the platform [system]. The arch is hardcoded to x86_64. *)
