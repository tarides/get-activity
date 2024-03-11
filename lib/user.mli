type t = Viewer | User of string

val query : Format.formatter -> t -> unit
(** Prints graphql query fragment that request the given user. *)

val response_field : t -> string
(** The field in the grphql response that contains the user fields. *)
