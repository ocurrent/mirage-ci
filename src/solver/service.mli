val v : n_workers:int -> create_worker:(unit -> Lwt_process.process) -> Solver_api.Solver.t
(** [v ~n_workers ~create_worker] is a solver service that distributes work to up to
    [n_workers] subprocesses, using [create_worker hash] to spawn new workers. *)
