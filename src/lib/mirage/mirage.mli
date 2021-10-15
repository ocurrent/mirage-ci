val build :
  config:Common.Config.t ->
  platform:Common.Platform.t ->
  base:Common.Spec.t Current.t ->
  project:Current_git.Commit_id.t Current.t ->
  unikernel:string ->
  target:string ->
  unit ->
  unit Current.t
(** Run the full mirage build process using ocluster. It includes the
    installation of mirage, the configuration step and the build step. *)
