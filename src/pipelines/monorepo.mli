(*
   ## MONOREPO PIPELINES
*)
open Mirage_ci_lib

val lock :
  system:Platform.system ->
  value:string ->
  ocluster:Current_ocluster.t ->
  store:Git_store.t ->
  monorepo:Monorepo.t Current.t ->
  repos:Repository.t list Current.t ->
  Universe.Project.t list ->
  Monorepo_lock.t Current.t
(** [lock ~system ~value ~monorepo ~repos projects] Obtain the lockfile of
    [projects] using the [monorepo] tool with the repositories [repos]. *)

val docs :
  system:Platform.system ->
  repos:Repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t

val released :
  ocluster:Current_ocluster.t ->
  platform:Platform.t ->
  roots:Universe.Project.t list ->
  repos:Repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the released resolution of the projects. *)

val mirage_edge :
  ocluster:Current_ocluster.t ->
  platform:Platform.t ->
  git_store:Git_store.t ->
  roots:Universe.Project.t list ->
  repos:Repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the main branches of [roots] projects, and released versions for
    everything else in the transitive dependency cone. *)

val universe_edge :
  ocluster:Current_ocluster.t ->
  platform:Platform.t ->
  git_store:Git_store.t ->
  roots:Universe.Project.t list ->
  repos:Repository.t list Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit Current.t
(** Test the main branches of every project in the dependency cone. *)
