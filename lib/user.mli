type t = Viewer | User of string

val query : Format.formatter -> t -> unit
(** Prints graphql query fragment that requests the given user. *)

val response_field : t -> string
(** The field in the graphql response that contains the user fields. *)
