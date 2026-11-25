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
  (ocaml (>= 5.5))
  menhir
  yojson
  ppx_yojson_conv
  ppx_enumerate
  astring
  digestif
  uucp
  toml
  (ppx_expect :with-test)
  (cmdliner (>= 2.0))
  mosaic
  (ocamlformat (and :with-dev-setup (= 0.29.0)))))
