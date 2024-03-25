open Get_activity

let ( / ) = Filename.concat

let or_die = function
  | Ok x -> x
  | Error (`Msg m) ->
      Fmt.epr "%s@." m;
      exit 1

let ( let* ) x y = y @@ or_die x

let home =
  match Sys.getenv_opt "HOME" with
  | None -> Fmt.failwith "$HOME is not set!"
  | Some dir -> dir

let ensure_dir_exists ~mode path =
  match Unix.stat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Unix.mkdir path mode
  | Unix.{ st_kind = S_DIR; _ } -> ()
  | _ -> Fmt.failwith "%S is not a directory!" path

let last_fetch_file =
  let dir = home / ".github" in
  ensure_dir_exists ~mode:0o700 dir;
  dir / "get-activity-timestamp"

let mtime path =
  match Unix.stat path with
  | info -> Some info.Unix.st_mtime
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None

let get_token () = Token.load (home / ".github" / "github-activity-token")

let show ~period ~user json =
  let* contribs = Contributions.of_json ~period ~user json in
  if Contributions.is_empty contribs then
    Fmt.epr "(no activity found since %s)@." (fst period)
  else Fmt.pr "%a@." Contributions.pp contribs

let mode = `Normal

open Cmdliner

let from =
  let doc =
    Arg.info ~docv:"TIMESTAMP" ~doc:"Starting date (ISO8601)." [ "from" ]
  in
  Arg.(value & opt (some string) None & doc)

let to_ =
  let doc = Arg.info ~docv:"TIMESTAMP" ~doc:"Ending date (ISO8601)." [ "to" ] in
  Arg.(value & opt (some string) None & doc)

let last_week =
  let doc = Arg.info ~doc:"Show activity from last week" [ "last-week" ] in
  Arg.(value & flag doc)

let period =
  let f from to_ last_week : Period.t =
    if last_week then `Last_week
    else
      match (from, to_) with
      | None, None -> `Since_last_fetch
      | Some x, Some y -> `Range (x, y)
      | _ -> Fmt.invalid_arg "--to and --from should be provided together"
  in
  Term.(const f $ from $ to_ $ last_week)

let user : User.t Term.t =
  let str_parser, str_printer = Arg.string in
  let parser x =
    match str_parser x with `Ok x -> `Ok (User.User x) | `Error e -> `Error e
  in
  let printer fs = function
    | User.Viewer -> str_printer fs "viewer"
    | User x -> str_printer fs x
  in
  let user_conv = (parser, printer) in
  let doc = Arg.info ~doc:"User name" [ "user" ] in
  Arg.(value & opt user_conv Viewer & doc)

let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let info = Cmd.info "get-activity" ~version

let run period user : unit =
  match mode with
  | `Normal ->
      Period.with_period period ~last_fetch_file ~f:(fun period ->
          (* Fmt.pr "period: %a@." Fmt.(pair string string) period; *)
          let* token = get_token () in
          let request = Contributions.request ~period ~user ~token in
          let* contributions = Graphql.exec request in
          show ~period ~user contributions)
  | `Save ->
      Period.with_period period ~last_fetch_file ~f:(fun period ->
          let* token = get_token () in
          let request = Contributions.request ~period ~user ~token in
          let* contributions = Graphql.exec request in
          Yojson.Safe.to_file "activity.json" contributions)
  | `Load ->
      (* When testing formatting changes, it is quicker to fetch the data once and then load it again for each test: *)
      let period =
        let from = mtime last_fetch_file |> Option.value ~default:0.0 in
        let to_ = Unix.time () in
        (Period.to_8601 from, Period.to_8601 to_)
      in
      show ~period ~user @@ Yojson.Safe.from_file "activity.json"

let term = Term.(const run $ period $ user)
let cmd = Cmd.v info term
let () = Stdlib.exit @@ Cmd.eval cmd
