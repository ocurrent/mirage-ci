(lang dune 2.0)

(name mirage-ci)

(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:

(source
 (github TheLortex/mirage-ci))

(authors "lucas@tarides.com")

(maintainers "lucas@tarides.com")

(package
 (name mirage-ci)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (current
   (>= 0.3))
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
  (ocaml
   (>= 4.08.0))))
