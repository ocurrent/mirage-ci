open Common

type build_mode =
  | Mirage_3
  | Mirage_4 of { overlay : Current_git.Commit_id.t Current.t }

val all_in_one_test :
  platform:Platform.t ->
  repos:Repository.t list Current.t ->
  mirage:Current_git.Commit_id.t option Current.t ->
  config:Config.t ->
  build_mode:build_mode ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton, all unikernels in one job *)
