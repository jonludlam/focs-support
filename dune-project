(lang dune 3.0)

(generate_opam_files true)

(name focs)

(source (github jonludlam/focs-support))

(authors "Jon Ludlam")
(maintainers "Jon Ludlam")
(license ISC)
(package
 (name focs)
 (synopsis "Support library for Foundations of Computer Science")
 (description "Some small libraries to help with the Jupyter notebooks used for the Cambridge 1A Computer Science Tripos course 'Foundations of Computer Science'")
 (depends ocaml dune fmt jupyter re))

