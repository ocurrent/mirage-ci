type t

val make : base:Current_git.Commit_id.t -> Current_git.Commit_id.t list -> t
(* Assumption: the commits are all on the same repository. *)

val no_merge : Current_git.Commit_id.t -> t
val spec : t -> Obuilder_spec.t
val to_list : t -> Current_git.Commit_id.t list
