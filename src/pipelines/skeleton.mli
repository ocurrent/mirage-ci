open Common

type 'a build_mode = Mirage_3 | Mirage_4 of { overlay : 'a option }

val all_in_one_test :
  platform:Platform.t ->
  repos:Merge_commit.t list Current.t ->
  mirage:Merge_commit.t option Current.t ->
  config:Config.t ->
  build_mode:Merge_commit.t Current.t build_mode ->
  Merge_commit.t Current.t ->
  unit Current.t
(** Test mirage-skeleton, all unikernels in one job *)
