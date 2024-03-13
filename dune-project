(lang dune 2.8)
(name get-activity)
(generate_opam_files true)
(source (github tarides/get-activity))
(authors "talex5@gmail.com")
(maintainers "Guillaume Petiot <guillaume@tarides.com>")

(package
 (name get-activity)
 (synopsis "Collect activity as markdown")
 (depends
  (cmdliner (>= 1.1.1))
  dune-build-info
  (get-activity-lib (= :version))))

(package
 (name get-activity-lib)
 (synopsis "Collect activity as markdown")
 (depends
  (alcotest :with-test)
  (ppx_expect :with-test)
  astring
  curly
  (fmt (>= 0.8.7))
  (yojson (>= 1.6))
  (ocaml (>= 4.08))))
