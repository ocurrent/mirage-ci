type t = { base : string; ops : Obuilder_spec.op list; children : (string * Obuilder_spec.t) list }

let add next_ops { base; ops; children } = { base; ops = ops @ next_ops; children }

let children ~name spec { base; ops; children } = { base; ops; children = (name, spec) :: children }

let finish { base; ops; children } = Obuilder_spec.stage ~child_builds:children ~from:base ops

let make base =
  let open Obuilder_spec in
  {
    base;
    ops = [ user ~uid:1000 ~gid:1000; workdir "/home/opam"; run "sudo chown opam:opam /home/opam" ];
    children = [];
  }
