module Json = Yojson.Safe

let ( / ) a b = Json.Util.member b a

let query =
  {| query($from: DateTime!, $to: DateTime!) {
   viewer {
    login
    contributionsCollection(from: $from, to: $to) {
      issueContributions(first: 100) {
        nodes {
          occurredAt
          issue {
            url
            title
            body
            repository { nameWithOwner }
          }
        }
      }
      pullRequestContributions(first: 100) {
        nodes {
          occurredAt
          pullRequest {
            url
            title
            body
            repository { nameWithOwner }
          }
        }
      }
      pullRequestReviewContributions(first: 100) {
        nodes {
          occurredAt
          pullRequestReview {
            url
            pullRequest { title }
            body
            state
            comments(first: 100) { nodes { body } }
            repository { nameWithOwner }
          }
        }
      }
      repositoryContributions(first: 100) {
        nodes {
          occurredAt
          repository {
            url
            nameWithOwner
          }
        }
      }
    }
  }
}|}

let fetch ~period:(start, finish) ~token =
  Lwt_main.run
    (let variables = [ ("from", `String start); ("to", `String finish) ] in
     Graphql.exec ~token ~variables ~query ())

module Datetime = struct
  type t = string

  let parse = function
    | `String s -> s
    | x -> Fmt.failwith "Invalid Datatime %a" Json.pp x
end

module Repo_map = Map.Make (String)

type item = {
  repo : string;
  kind : [ `Issue | `PR | `Review of string | `New_repo ];
  date : Datetime.t;
  url : string;
  title : string;
  body : string;
}

type t = { username : string; activity : item list Repo_map.t }

let read_issues json =
  Json.Util.to_list (json / "nodes")
  |> List.filter (( <> ) `Null)
  |> List.map @@ fun node ->
     let date = Datetime.parse (node / "occurredAt") in
     let x = node / "issue" in
     let url = x / "url" |> Json.Util.to_string in
     let title = x / "title" |> Json.Util.to_string in
     let body = x / "body" |> Json.Util.to_string in
     let repo = x / "repository" / "nameWithOwner" |> Json.Util.to_string in
     { kind = `Issue; date; url; title; body; repo }

let read_prs json =
  Json.Util.to_list (json / "nodes")
  |> List.filter (( <> ) `Null)
  |> List.map @@ fun node ->
     let date = Datetime.parse (node / "occurredAt") in
     let pr = node / "pullRequest" in
     let url = pr / "url" |> Json.Util.to_string in
     let title = pr / "title" |> Json.Util.to_string in
     let body = pr / "body" |> Json.Util.to_string in
     let repo = pr / "repository" / "nameWithOwner" |> Json.Util.to_string in
     { kind = `PR; date; url; title; body; repo }

let read_reviews json =
  Json.Util.to_list (json / "nodes")
  |> List.filter (( <> ) `Null)
  |> List.map @@ fun node ->
     let date = Datetime.parse (node / "occurredAt") in
     let review = node / "pullRequestReview" in
     let state = review / "state" |> Json.Util.to_string in
     let url = review / "url" |> Json.Util.to_string in
     let pr = review / "pullRequest" in
     let title = pr / "title" |> Json.Util.to_string in
     let body = review / "body" |> Json.Util.to_string in
     let repo =
       review / "repository" / "nameWithOwner" |> Json.Util.to_string
     in
     { kind = `Review state; date; url; title; body; repo }

let read_repos json =
  Json.Util.to_list (json / "nodes")
  |> List.filter (( <> ) `Null)
  |> List.map @@ fun node ->
     let date = Datetime.parse (node / "occurredAt") in
     let repo = node / "repository" in
     let url = repo / "url" |> Json.Util.to_string in
     let repo = repo / "nameWithOwner" |> Json.Util.to_string in
     {
       kind = `New_repo;
       date;
       url;
       title = "Created new repository";
       body = "";
       repo;
     }

let of_json ~from json =
  let username = json / "data" / "viewer" / "login" |> Json.Util.to_string in
  let contribs = json / "data" / "viewer" / "contributionsCollection" in
  let items =
    read_issues (contribs / "issueContributions")
    @ read_prs (contribs / "pullRequestContributions")
    @ read_reviews (contribs / "pullRequestReviewContributions")
    @ read_repos (contribs / "repositoryContributions")
  in
  let activity =
    (* GitHub seems to ignore the time part, so do the filtering here. *)
    items
    |> List.filter (fun item -> item.date >= from)
    |> List.fold_left
         (fun acc item ->
           let items =
             Repo_map.find_opt item.repo acc |> Option.value ~default:[]
           in
           Repo_map.add item.repo (item :: items) acc)
         Repo_map.empty
  in
  { username; activity }

let id url =
  match Astring.String.cut ~sep:"/" ~rev:true url with
  | None -> Fmt.failwith "Invalid URL %S" url
  | Some (_, id) -> (
      match Astring.String.cut ~sep:"#" id with
      | Some (id, _) -> id
      | None -> id)

let pp_title f t =
  match t.kind with
  | `Issue -> Fmt.pf f "%s [#%s](%s)" t.title (id t.url) t.url
  | `PR -> Fmt.pf f "%s [#%s](%s)" t.title (id t.url) t.url
  | `Review s -> Fmt.pf f "%s %s [#%s](%s)" s t.title (id t.url) t.url
  | `New_repo -> (
      match Astring.String.cuts ~sep:"/" t.url |> List.rev with
      | repo :: org :: _ ->
          Fmt.pf f "Created repository [%s/%s](%s)" org repo t.url
      | _ -> Fmt.failwith "Malformed URL %S" t.url)

let pp_body f = function
  | "" -> ()
  | body ->
      let body =
        body |> String.split_on_char (Char.chr 13) |> String.concat ""
      in
      Fmt.pf f "  @,@[<hov>%a@]" Fmt.text body

let pp_item f t = Fmt.pf f "@[<v>%a.%a@]" pp_title t pp_body t.body
let pp_items = Fmt.(list ~sep:(cut ++ cut) pp_item)
let pp_repo f (name, items) = Fmt.pf f "### %s@,@,%a" name pp_items items
let is_empty { activity; _ } = Repo_map.is_empty activity

let pp f { activity; _ } =
  let by_repo = Repo_map.bindings activity in
  match by_repo with
  | [] -> Fmt.string f "(no activity)"
  | [ (_, items) ] -> pp_items f items
  | repos -> Fmt.(list ~sep:(cut ++ cut)) pp_repo f repos
