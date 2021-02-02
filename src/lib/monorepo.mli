val lock : base:Spec.t Current.t -> opam:Opamfile.t Current.t -> Monorepo_lock.t Current.t

val monorepo_main :
  roots:Universe.Project.t list ->
  base:Spec.t Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit ->
  Spec.t Current.t

val monorepo_released :
  roots:Universe.Project.t list ->
  base:Spec.t Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit ->
  Spec.t Current.t

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
