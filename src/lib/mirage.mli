val build :
  ocluster:Current_ocluster.t ->
  platform:Platform.t ->
  base:Spec.t Current.t ->
  project:Current_git.Commit_id.t Current.t ->
  unikernel:string ->
  target:string ->
  unit ->
  unit Current.t
(** Run the full mirage build process using ocluster. It includes the installation of mirage, the 
configuration step and the build step. *)
