(lang dune 3.17)
(using menhir 3.0)
(name rupam)
(generate_opam_files true)
(license GPL-3.0-or-later)
(authors "Visruth Srimath Kandali <public@visruth.com>")
(maintainers "Visruth Srimath Kandali <public@visruth.com>")
(source (github VisruthSK/rupam))
(package
 (name rupam)
 (synopsis "A typed language for interactive data analysis")
 (depends
  (ocaml (>= 5.4))
  menhir
  yojson
  ppx_yojson_conv
  ppx_enumerate
  ppx_sexp_conv
  sexplib0
  astring
  re
  digestif
  ptime
  uucp
  toml
  (ppx_expect :with-test)
  (qcheck-core :with-test)
  (cmdliner (= 2.1.2))
  (conf-mingw-w64-g++-x86_64 (= :os "win32"))
  mosaic
  matrix
  (ocamlformat (and :with-dev-setup (= 0.29.0)))))

(package
 (name rupam-dev)
 (synopsis "Internal tools for developing Rupam")
 (allow_empty)
 (depends rupam))
