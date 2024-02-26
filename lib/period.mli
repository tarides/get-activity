type t =
  [ `Last_week
  | `Range of string * string 
  | `Since_last_fetch ]

val one_week : float

val to_8601 : float -> string

val with_period : t -> last_fetch_file:string -> f:(string * string -> unit) -> unit
(** Run [f (start, finish)], where [(start, finish)] is the period specified by [period].
   If [period] is [`Since_last_fetch] or [`Last_week] then update the last-fetch timestamp on success. *)
