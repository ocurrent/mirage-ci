val add_repositories : Repository.t list -> Obuilder_spec.op list
(** The obuilder rules to add opam repositories. *)

val install_tools : string list -> Obuilder_spec.op list
(** The obuilder rules to opam install the given list of tools. *)

val opam_download_cache : Obuilder_spec.Cache.t
(** Obuilder cache for opam downloads *)

val dune_build_cache : Obuilder_spec.Cache.t
(** Obuilder cache for dune builds *)

val remote_uri : Current_git.Commit_id.t -> string
(** Get the opam-compatible URI of the commit. *)

val network : string list
