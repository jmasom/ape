(lang dune 3.16)

(name ape)

(generate_opam_files true)

(source
 (github jmasom/ape))

(authors "Joseph Masom <dajaysta@gmail.com>")

(maintainers "Joseph Masom <dajaysta@gmail.com>")

(license MIT)

(documentation https://github.com/jmasom/ape)

(package
 (name ape)
 (synopsis "Yet another random word generator")
 (description "Ape is a random word generator targeted to conlangers. It supports matchable rules, phonological features, and sound changes.")
 (depends
  ocaml
  dune
  (atdgen
   (>= 2.15.0))
  (ocamlformat
   (and
    (>= 0.26.2)
    :build)))
 (tags
  (topics "word generation" conlanging)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
