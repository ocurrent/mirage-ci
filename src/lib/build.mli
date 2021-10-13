module Mirage_3 : sig
  val run :
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
end

module Mirage_4 : sig
  type stage = Configure | Depends | Lock | Pull | Build | Clean [@@deriving yojson]

  val name : stage -> string

  val run :
    ocluster:Current_ocluster.t ->
    platform:Platform.t ->
    base:Spec.t Current.t ->
    project:Current_git.Commit_id.t Current.t ->
    target:string ->
    stage ->
    unit Current.t
  (** Run the full mirage build process using ocluster. It includes the installation of mirage, the
configuration step and the build step. *)
end
