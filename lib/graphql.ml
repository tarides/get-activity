let ( let* ) = Result.bind
let ( / ) a b = Yojson.Safe.Util.member b a

module Request = struct
  type t = { request : Cohttp.Request.t; uri : Uri.t; body : Cohttp_eio.Body.t }

  let make ?variables ~token ~query () =
    let body =
      `Assoc
        (("query", `String query)
        ::
        (match variables with
        | None -> []
        | Some v -> [ ("variables", `Assoc v) ]))
      |> Yojson.Safe.to_string |> Cohttp_eio.Body.of_string
    in
    let uri = Uri.of_string "https://api.github.com/graphql" in
    let meth = `POST in
    let headers = Cohttp.Header.init_with "Authorization" ("bearer " ^ token) in
    let request = Cohttp.Request.make ~meth ~headers uri in
    { request; uri; body }

  let exec client sw { request; body; uri } =
    Logs.debug (fun m -> m "request: @[%a@]@." Cohttp.Request.pp_hum request);
    let headers = request.headers in
    let resp, body = Cohttp_eio.Client.post ~sw ~body ~headers client uri in
    match resp.status with
    | `OK -> (
        Logs.debug (fun m -> m "response: @[%a@]@." Http.Response.pp resp);
        let* body = (Eio.Buf_read.(parse take_all) body) ~max_size:max_int in
        let json = Yojson.Safe.from_string body in
        match json / "message" with
        | `Null -> Ok json
        | `String e ->
            Error
              (`Msg (Format.asprintf "@[<v2>GitHub returned errors: %s@]" e))
        | _errors ->
            Error
              (`Msg
                (Format.asprintf "@[<v2>GitHub returned errors: %a@]"
                   (Yojson.Safe.pretty_print ~std:true)
                   json)))
    | status ->
        Error
          (`Msg
            (Fmt.str
               "@[<v2>Error performing GraphQL query on GitHub: Unexpected \
                HTTP status %a@]"
               Http.Status.pp status))

  let pp ppf { request; uri = _; body = _ } =
    let pp_request ppf r =
      Fmt.pf ppf "@[<v>request =@;<1 2>@[<v2>%a@]@]" Cohttp.Request.pp_hum r
    in
    let pp_body ppf () = Fmt.pf ppf "@[<v>body =@;<1 2><...>@]" in
    Fmt.pf ppf "@[<v2>{@ %a;@ %a@ }@]" pp_request request pp_body ()
end
