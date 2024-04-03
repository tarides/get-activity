module Datetime : sig
  type t = string
end

type item = {
  repo : string;
  kind :
    [ `Issue
    | `PR
    | `Comment of [ `Issue | `PR ]
    | `Review of string
    | `Merge
    | `New_repo ];
  date : Datetime.t;
  url : string;
  title : string;
  body : string;
}

module Repo_map : Map.S with type key = string

type t = { username : string; activity : item list Repo_map.t }

val request :
  period:string * string -> user:User.t -> token:Token.t -> Graphql.request

val of_json :
  period:string * string ->
  user:User.t ->
  Yojson.Safe.t ->
  (t, [ `Msg of string ]) result
(** We pass [period] again here so we can filter out anything that GitHub included by accident. *)

val is_empty : t -> bool

val pp : t Fmt.t
(** [pp] formats as markdown. *)
