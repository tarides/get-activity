let ( / ) a b = Yojson.Safe.Util.member b a

type request = {
  meth : Curly.Meth.t;
  url : string;
  headers : Curly.Header.t;
  body : Yojson.Safe.t;
}

let request ?variables ~token ~query () =
  let body =
    `Assoc
      (("query", `String query)
      ::
      (match variables with
      | None -> []
      | Some v -> [ ("variables", `Assoc v) ]))
  in
  let url = "https://api.github.com/graphql" in
  let headers = [ ("Authorization", "bearer " ^ token) ] in
  { meth = `POST; url; headers; body }

let exec request =
  let { meth; url; headers; body } = request in
  let body = Yojson.Safe.to_string body in
  let request = Curly.Request.make ~headers ~body ~url ~meth () in
  match Curly.run request with
  | Ok { Curly.Response.body; _ } -> (
      let json = Yojson.Safe.from_string body in
      match json / "message" with
      | `Null -> Ok json
      | `String e ->
          Error (`Msg (Format.asprintf "@[<v2>GitHub returned errors: %s@]" e))
      | _errors ->
          Error
            (`Msg
              (Format.asprintf "@[<v2>GitHub returned errors: %a@]"
                 (Yojson.Safe.pretty_print ~std:true)
                 json)))
  | Error e ->
      Error
        (`Msg
          (Format.asprintf
             "@[<v2>Error performing GraphQL query on GitHub: %a@]"
             Curly.Error.pp e))
