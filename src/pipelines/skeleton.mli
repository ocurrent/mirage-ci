val v :
  build:Mirage_lib.Mirage.mirage_builder ->
  config:Common.Config.t ->
  platform:Common.Platform.t ->
  repos:Common.Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)

type test_options

val test_options_cmdliner : test_options Cmdliner.Term.t

val local :
  config:Common.Config.t ->
  options:test_options ->
  ((string * string) list -> Common.Repository.t list Current.t) ->
  unit Current.t
