open Mirage_ci_lib

val v_3 :
  ocluster:Current_ocluster.t ->
  platform:Platform.t ->
  mirage:Current_git.Commit_id.t Current.t ->
  repos:Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)

val v_4 :
  ocluster:Current_ocluster.t ->
  repos:Repository.fetched list Current.t ->
  platform:Platform.t ->
  targets:string list ->
  Current_git.Commit.t Current.t ->
  unit Current.t
(** Pipeline optimized for mirage 4, using opam-monorepo to track if
resolutions changes. *)
