type request = {
  meth : Curly.Meth.t;
  url : string;
  headers : Curly.Header.t;
  body : Yojson.Safe.t;
}

val request :
  ?variables:(string * Yojson.Safe.t) list ->
  token:string ->
  query:string ->
  unit ->
  request

val exec : request -> (Yojson.Safe.t, [ `Msg of string ]) result
