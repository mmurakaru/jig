(lang dune 3.24)
(name jig)

(generate_opam_files true)

(source (github mmurakaru/jig))
(authors "Markus Murakaru")
(maintainers "Markus Murakaru")
(license MIT)

(package
 (name jig)
 (synopsis "A minimal, agnostic runner for AI-driven development workflows")
 (description
  "Declarative workflow format plus a portable runner with four swappable ports (Executor, ModelProvider, Metering, Store). The primitive, not the product.")
 (depends
  (ocaml (>= 5.1))
  (yaml (>= 3.0))
  (yojson (>= 2.0))
  (cmdliner (>= 1.3))
  (alcotest (and :with-test (>= 1.7)))))
