module Request : sig
  type t

  val make :
    ?variables:(string * Yojson.Safe.t) list ->
    token:string ->
    query:string ->
    unit ->
    t

  val exec :
    Cohttp_eio.Client.t ->
    Eio.Switch.t ->
    t ->
    (Yojson.Safe.t, [ `Msg of string ]) result

  val pp : t Fmt.t
end
