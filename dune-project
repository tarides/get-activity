(lang dune 2.8)

(name get-activity)

(generate_opam_files true)

(source
 (github tarides/get-activity))

(authors "talex5@gmail.com")

(maintainers "Guillaume Petiot <guillaume@tarides.com>")

(package
 (name get-activity)
 (synopsis "Collect activity as markdown")
 (depends
  cohttp-eio
  (cmdliner
   (>= 1.1.1))
  dune-build-info
  eio_main
  fmt
  logs
  mirage-crypto-rng-eio
  (tls-eio
   (>= 1.0.0))
  (get-activity-lib
   (= :version))))

(package
 (name get-activity-lib)
 (synopsis "Collect activity as markdown")
 (depends
  (alcotest :with-test)
  (ppx_expect :with-test)
  astring
  cohttp-eio
  (fmt
   (>= 0.8.7))
  logs
  ppx_yojson_conv
  (yojson
   (>= 1.6))
  (ocaml
   (>= 5.0))))
