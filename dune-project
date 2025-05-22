(lang dune 3.14)

(name c_of_ocaml)

(using experimental_building_ocaml_compiler_with_dune 0.1)

(generate_opam_files true)

(source
 (github nathanfarlow/c-of-ocaml))

(authors "Nathan Farlow")

(maintainers "Nathan Farlow")

(license LICENSE)

(documentation "https://github.com/nathanfarlow/c-of-ocaml/")

(package
 (name c_of_ocaml)
 (synopsis "Compiles an OCaml program to a standalone ANSI C file")
 (description "See https://farlow.dev/2025/05/17/ocaml-on-calculator for more information.")
 (depends
  ocaml
  dune
  (js_of_ocaml-compiler
   (>= 5.6.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
