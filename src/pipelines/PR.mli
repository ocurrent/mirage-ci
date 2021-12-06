module Github = Current_github
module Git = Current_git

type t =
  ( unit,
    string,
    string,
    Website.Website_description.Pipeline.t )
  Current_web_pipelines.State.pipeline

(** The PR tester *)

type test_options
(** Test options (enabling commit status, disabling tasks) *)

val test_options_cmdliner : test_options Cmdliner.Term.t
val is_enabled : test_options -> bool

val make :
  config:Common.Config.t ->
  options:test_options ->
  repos:(string * Git.Commit_id.t) list Current.t ->
  Github.Api.t ->
  (unit, t list) Current_web_pipelines.Task.t

val local :
  config:Common.Config.t ->
  options:test_options ->
  repos:(string * Git.Commit_id.t) list Current.t ->
  (unit, t list) Current_web_pipelines.Task.t
