(lang dune 3.18)

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
  (ocaml
   (= 5.1.1))
  (atdgen
   (>= 2.15.0)))
 (tags
  (topics "word generation" conlanging)))

(using menhir 3.0)
