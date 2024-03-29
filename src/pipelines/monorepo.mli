(*
   ## MONOREPO PIPELINES
*)
open Monorepo_lib
open Common

val lock :
  system:Platform.system ->
  value:string ->
  config:Config.t ->
  store:Monorepo.Git_store.t ->
  monorepo:Monorepo.t Current.t ->
  repos:Opam_repository.t list Current.t ->
  Universe.Project.t list ->
  Monorepo_lock.t Current.t
(** [lock ~system ~value ~monorepo ~repos projects] Obtain the lockfile of
    [projects] using the [monorepo] tool with the repositories [repos]. *)

val released :
  config:Config.t ->
  platform:Platform.t ->
  roots:Universe.Project.t list ->
  repos:Opam_repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the released resolution of the projects. *)

val mirage_edge :
  config:Config.t ->
  platform:Platform.t ->
  git_store:Git_store.t ->
  roots:Universe.Project.t list ->
  repos:Opam_repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the main branches of [roots] projects, and released versions for
    everything else in the transitive dependency cone. *)

val universe_edge :
  config:Config.t ->
  platform:Platform.t ->
  git_store:Git_store.t ->
  roots:Universe.Project.t list ->
  repos:Opam_repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the main branches of every project in the dependency cone. *)
