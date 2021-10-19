open Common

val v :
  build:Mirage_lib.Mirage.mirage_builder ->
  config:Config.t ->
  platform:Platform.t ->
  mirage:Current_git.Commit_id.t Current.t ->
  repos:Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)

val all_in_one_test :
  platform:Platform.t ->
  repos:Repository.t list Current.t ->
  mirage_overlay:Current_git.Commit_id.t Current.t ->
  config:Config.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton, all unikernels in one job *)
