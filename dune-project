(lang dune 3.17)
(using menhir 3.0)
(name rupam)
(generate_opam_files true)
(package
 (name rupam)
 (synopsis "A typed language for interactive data analysis")
 (depends (ocaml (>= 5.1)) menhir yojson uucp))
