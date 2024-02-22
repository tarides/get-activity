(lang dune 2.3)
(name get-activity)
(formatting disabled)
(generate_opam_files true)
(source (github tarides/get-activity))
(authors "talex5@gmail.com")
(maintainers "Guillaume Petiot <guillaume@tarides.com>")
(package
 (name get-activity)
 (synopsis "collect activity as markdown")
 (depends
  (cmdliner (>= 1.1.1))
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  yojson
  (ocaml (>= 4.08))))
