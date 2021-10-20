module Github = Current_github
module Git = Current_git
open Common

type t
(** The PR tester *)

type test_options
(** Test options (enabling commit status, disabling tasks) *)

val test_options_cmdliner : test_options Cmdliner.Term.t
val is_enabled : test_options -> bool

val make :
  config:Config.t ->
  options:test_options ->
  repos:Merge_commit.t list Current.t ->
  Github.Api.t ->
  t

val to_current : t -> unit Current.t
val routes : t -> Current_web.Resource.t Routes.route list

val local :
  config:Config.t ->
  options:test_options ->
  repos:Merge_commit.t list Current.t ->
  unit Current.t
