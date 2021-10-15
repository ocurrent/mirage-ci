val v :
  build:Mirage_lib.Mirage.mirage_builder ->
  config:Common.Config.t ->
  platform:Common.Platform.t ->
  mirage:Current_git.Commit_id.t Current.t ->
  repos:Common.Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)
