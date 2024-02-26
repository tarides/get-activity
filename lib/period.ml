type t =
  [ `Last_week
  | `Range of string * string 
  | `Since_last_fetch ]

let one_week = 60. *. 60. *. 24. *. 7.

let to_8601 t =
  let open Unix in
  let t = gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (t.tm_year + 1900)
    (t.tm_mon + 1)
    (t.tm_mday)
    (t.tm_hour)
    (t.tm_min)
    (t.tm_sec)

let mtime path =
  match Unix.stat path with
  | info -> Some info.Unix.st_mtime
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> None

let set_mtime path time =
  if not (Sys.file_exists path) then
    close_out @@ open_out_gen [Open_append; Open_creat] 0o600 path;
  Unix.utimes path time time

let with_period period ~last_fetch_file ~f =
  let now = Unix.time () in
  let last_week = now -. one_week in
  let range =
    match period with
    | `Since_last_fetch ->
      let last_fetch = Option.value ~default:last_week (mtime last_fetch_file) in
      (to_8601 last_fetch, to_8601 now)
    | `Last_week ->
      (to_8601 last_week, to_8601 now)
    | `Range r -> r
  in
  f range;
  match period with
  | `Since_last_fetch | `Last_week -> set_mtime last_fetch_file now
  | `Range _ -> ()
