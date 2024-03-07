val exec :
  ?variables:(string * Yojson.Safe.t) list ->
  token:string ->
  query:string ->
  unit ->
  (Yojson.Safe.t, [ `Msg of string ]) result Lwt.t
