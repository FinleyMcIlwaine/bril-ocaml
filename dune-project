(lang dune 2.8)
(name bril)
(version 0.1.0)
(maintainers "Finley McIlwaine")
(authors "Finley McIlwaine" "CS6120 @ Cornell")


(generate_opam_files true)

(package
 (name bril)
 (synopsis "A library for loading and emitting Bril programs.")
 (depends
  (yojson (>= 1.7.0))
  (core (>= v0.14.1))))
