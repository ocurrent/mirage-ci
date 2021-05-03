module Github = Current_github
module Git = Current_git

module RepoBranch : sig
  type t = Skeleton_master | Mirage_3 | Skeleton_dev | Mirage_master | Mirage_dev_master

  val to_string : t -> string

  val of_string : string -> t option

  val all : t list
end

type t
(** The PR tester *)

val make :
  ocluster:Current_ocluster.t ->
  test:RepoBranch.t list ->
  commit_status:RepoBranch.t list ->
  Github.Api.t ->
  (string * Git.Commit_id.t) list Current.t ->
  t

val to_current : t -> unit Current.t

val routes : t -> Current_web.Resource.t Routes.route list
