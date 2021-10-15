val v_main :
  config:Common.Config.t ->
  platform:Common.Platform.t ->
  mirage:Current_git.Commit_id.t Current.t ->
  repos:Common.Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)

val v_4 :
  config:Common.Config.t ->
  repos:Common.Repository.fetched list Current.t ->
  platform:Common.Platform.t ->
  targets:string list ->
  Current_git.Commit.t Current.t ->
  unit Current.t
(** Pipeline optimized for mirage 4, using opam-monorepo to track if resolutions
    changes. *)
