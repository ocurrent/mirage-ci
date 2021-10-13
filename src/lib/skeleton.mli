module Mirage_3 : sig
  type stage = string * string * string list
  (** name, root folder, unikernel folder *)

  val stages : Current_git.Commit.t Current.t -> stage list Current.t
end

module Mirage_4 : sig
  type stage = Build.Mirage_4.stage

  val stages : Current_git.Commit.t Current.t -> stage list Current.t
end
