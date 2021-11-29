open Common

type 'a build_mode = Mirage_3 | Mirage_4 of { overlay : 'a option }

val all_in_one_test :
  platform:Platform.t ->
  repos:Repository.t list Current.t ->
  mirage:Current_git.Commit_id.t option Current.t ->
  config:Config.t ->
  build_mode:Current_git.Commit_id.t Current.t build_mode ->
  Current_git.Commit_id.t Current.t ->
  ( unit,
    (unit, string, string) Current_web_pipelines.State.stage )
  Current_web_pipelines.Task.t
(** Test mirage-skeleton, all unikernels in one job *)
