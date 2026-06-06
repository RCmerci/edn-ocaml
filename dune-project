(lang dune 3.16)

(using melange 0.1)

(name edn_ocaml)

(generate_opam_files true)

(package
 (name edn_ocaml)
 (synopsis "EDN reader and writer for OCaml")
 (depends
  (ocaml (>= 5.0))
  melange
  (melange-jest :with-test)))
