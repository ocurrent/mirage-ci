(lang dune 2.0)
(name mirage-ci)
(generate_opam_files true)
(source (github TheLortex/mirage-ci))
(authors "lucas@tarides.com")
(maintainers "lucas@tarides.com")
(package
 (name mirage-ci)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (uri (>= 4.2.0))
  (tyxml (>= 4.5.0))
  (stdint (>= 0.7.0))
  (routes (>= 1.0.0))
  (result (>= 1.5))
  (ppx_deriving_yojson (>= 3.6.1))
  (ppx_deriving (>= 5.2.1))
  (opam-state (>= 2.1.0))
  (opam-core (>= 2.1.0))
  (mirage-flow (>= 2.0.1))
  (mirage-crypto (>= 0.10.3))
  (logs (>= 0.7.0))
  (git (>= 3.5.0))
  (fpath (>= 0.7.3))
  (duration (>= 0.2.0))
  (digestif (>= 1.0.1))
  (cstruct (>= 6.0.1))
  (capnp-rpc-net (>= 1.2))
  (capnp-rpc-lwt (>= 1.2))
  (capnp-rpc (>= 1.2))
  (capnp (>= 3.4.0))
  (bos (>= 0.2.1))
  (astring (>= 0.8.5))
  (current (>= 0.3))
  current_web
  current_git
  fmt
  lwt
  cmdliner
  opam-file-format
  lambdasoup
  dockerfile
  obuilder-spec
  opam-0install
  capnp-rpc-unix
  opam-format
  git-unix
  (ocaml (>= 4.08.0))))
