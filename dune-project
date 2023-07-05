(lang dune 3.7)
(name mirage-ci)

(generate_opam_files true)
(source (github ocurrent/mirage-ci))
(authors "Lucas Pluvinage <lucas@tarides.com>" "Thomas Gazagnaire <thomas@gazagnaire.org>" "Tim McGilchrist <timmcgil@gmail.com>" "Thomas Leonard <talex5@gmail.com>" "benmandrew <benmandrew@gmail.com>" "Navin Keswani <navin@novemberkilo.com>" "Romain Calascibetta <romain.calascibetta@gmail.com>")
(maintainers "alpha@tarides.com" "Tim McGilchrist <timmcgil@gmail.com>")

(package
 (name mirage-ci)
 (synopsis "OCurrent pipelines for testing MirageOS")
 (description "This CI is a set of OCurrent pipelines testing various things for the Mirage project.")
 (depends
  (ocaml (>= 4.14.1))
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
  (capnp-rpc-net (>= 1.2.3))
  (capnp-rpc-lwt (>= 1.2.3))
  (capnp-rpc (>= 1.2.3))
  (capnp (>= 3.4.0))
  (bos (>= 0.2.1))
  (astring (>= 0.8.5))
  (current (>= 0.6.4))
  current_web
  current_git
  current_github
  (current_ocluster (>= 0.2))
  current_rpc
  current_docker
  fmt
  lwt
  (cmdliner (>= 0.9.8))
  opam-file-format
  lambdasoup
  dockerfile
  (obuilder-spec (>= 0.5))
  opam-0install
  (capnp-rpc-unix (>= 1.2.3))
  opam-format
  git-unix))
