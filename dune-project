(lang dune 3.14)

(name c_of_ocaml)

(using experimental_building_ocaml_compiler_with_dune 0.1)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name c_of_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (js_of_ocaml-compiler
   (>= 5.6.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
