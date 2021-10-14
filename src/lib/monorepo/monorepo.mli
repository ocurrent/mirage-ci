type t
(** The opam monorepo tool*)

val v :
  system:Common.Platform.system ->
  repos:Common.Repository.fetched list Current.t ->
  t Current.t
(** Build the opam monorepo tool on [system] using [repos]. *)

val lock :
  key:string ->
  value:string ->
  cluster:Current_ocluster.t ->
  store:Git_store.t ->
  repos:Common.Repository.t list Current.t ->
  opam:Opamfile.t Current.t ->
  system:Common.Platform.system ->
  t Current.t ->
  Monorepo_lock.t Current.t
(** Perform `opam monorepo lock` on the given [opam] definition using [repos]. *)

val spec :
  base:Common.Spec.t Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit ->
  Common.Spec.t Current.t
(** Use opam-monorepo to fetch the lockfile in /src/duniverse *)

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
(** Get the opam file describing a set of projects. *)

module Git_store = Git_store
