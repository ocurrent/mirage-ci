type stage = string * string * string list (* name, root folder, unikernel folder *)

type stages = stage list

val stages : Current_git.Commit.t Current.t -> stages Current.t
