module Github = Current_github
module Git = Current_git

type t
(** The PR tester *)

type test_options
(** Test options (enabling commit status, disabling tasks) *)

val test_options_cmdliner : test_options Cmdliner.Term.t

val is_enabled : test_options -> bool

val make :
  ocluster:Current_ocluster.t ->
  options:test_options ->
  Github.Api.t ->
  (string * Git.Commit_id.t) list Current.t ->
  t

val to_current : t -> unit Current.t

val routes : t -> Current_web.Resource.t Routes.route list
