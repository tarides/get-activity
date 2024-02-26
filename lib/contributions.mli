type t

module Datetime : sig
  type t = string
end

type item = {
  repo : string;
  kind : [`Issue | `PR | `Review of string | `New_repo ];
  date: Datetime.t;
  url : string;
  title : string;
  body : string;
}

module Repo_map : Map.S with type key = string

val fetch : period:(string * string) -> token:Token.t -> Yojson.Safe.t

val of_json : from:string -> Yojson.Safe.t -> t
(** We pass [from] again here so we can filter out anything that GitHub included by accident. *)

val is_empty : t -> bool

val pp : t Fmt.t
(** [pp] formats as markdown. *)
