type t = Viewer | User of string

let query fs = function
  | User u -> Format.fprintf fs "user(login: %S)" u
  | Viewer -> Format.fprintf fs "viewer"

let response_field = function User _ -> "user" | Viewer -> "viewer"
