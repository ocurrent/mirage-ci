open Common

val all_in_one_test :
  platform:Platform.t ->
  repos:Repository.t list Current.t ->
  mirage:Current_git.Commit_id.t option Current.t ->
  mirage_overlay:Current_git.Commit_id.t Current.t ->
  config:Config.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton, all unikernels in one job *)
