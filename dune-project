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
  (get-activity (= :version))))

(package
 (name get-activity-lib)
 (synopsis "Collect activity as markdown")
 (depends
  cohttp
  cohttp-lwt
  cohttp-lwt-unix
  yojson
  (ocaml (>= 4.08))))
