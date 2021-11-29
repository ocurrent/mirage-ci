module Github = Current_github
module Git = Current_git

type metadata_gh = {
  kind : [ `Mirage | `Mirage_dev | `Mirage_skeleton | `Opam_overlays ];
  build_mode : [ `Mirage_4 | `Mirage_3 ];
  commit : string;
  ref : Github.Api.Ref.t;
  owner : string;
  name : string;
  friend_prs : Github.Api.Ref.pr_info list;
}

type pipeline = [ `Local of [ `Mirage_4 | `Mirage_3 ] | `Github of metadata_gh ]
type t = (unit, string, string, pipeline) Current_web_pipelines.State.pipeline


val id : pipeline -> string

val compare_metadata : pipeline -> pipeline -> int

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
