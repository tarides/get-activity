module Json = Yojson.Safe

let ( / ) a b = Json.Util.member b a

let query user =
  Format.asprintf
    {|query($from: DateTime!, $to: DateTime!) {
   %a {
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
    User.query user

let request ~period:(start, finish) ~user ~token =
  let variables = [ ("from", `String start); ("to", `String finish) ] in
  let query = query user in
  Graphql.request ~token ~variables ~query ()

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

let of_json ~from ~user json =
  let username =
    json / "data" / User.response_field user / "login" |> Json.Util.to_string
  in
  let contribs =
    json / "data" / User.response_field user / "contributionsCollection"
  in
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
  | `New_repo -> Fmt.pf f "Created repository [%s](%s)" t.repo t.url

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

let pp fs t = Fmt.pf fs "@[<v>%a@]" pp t

let%expect_test "Contributions.pp" =
  let contributions_example = { username = "me"; activity = Repo_map.empty } in
  Format.printf "%a" pp contributions_example;
  [%expect {|
    (no activity) |}]

let contributions_example =
  {
    username = "me";
    activity =
      Repo_map.empty
      |> Repo_map.add "gpetiot/js_of_ocaml"
           [
             {
               repo = "gpetiot/js_of_ocaml";
               kind = `New_repo;
               date = "2024-03-01T10:43:33Z";
               url = "https://github.com/gpetiot/js_of_ocaml";
               title = "Title1";
               body = "";
             };
           ]
      |> Repo_map.add "realworldocaml/mdx"
           [
             {
               repo = "realworldocaml/mdx";
               kind = `Review "APPROVED";
               date = "2024-03-05T11:43:04Z";
               url =
                 "https://github.com/realworldocaml/mdx/pull/449#pullrequestreview-1916654244";
               title = "Title2";
               body = "xxx";
             };
             {
               repo = "realworldocaml/mdx";
               kind = `PR;
               date = "2024-03-04T17:20:11Z";
               url = "https://github.com/realworldocaml/mdx/pull/450";
               title = "Title3";
               body = "xxx";
             };
           ]
      |> Repo_map.add "tarides/okra"
           [
             {
               repo = "tarides/okra";
               kind = `Review "APPROVED";
               date = "2024-02-28T11:09:41Z";
               url =
                 "https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361";
               title = "Title4";
               body = "xxx";
             };
             {
               repo = "tarides/okra";
               kind = `Issue;
               date = "2024-02-27T12:05:04Z";
               url = "https://github.com/tarides/okra/issues/165";
               title = "Title5";
               body = "xxx";
             };
           ];
  }

let%expect_test "Contributions.pp" =
  Format.printf "%a" pp contributions_example;
  [%expect
    {|
    ### gpetiot/js_of_ocaml

    Created repository [gpetiot/js_of_ocaml](https://github.com/gpetiot/js_of_ocaml).

    ### realworldocaml/mdx

    APPROVED Title2 [#449](https://github.com/realworldocaml/mdx/pull/449#pullrequestreview-1916654244).
    xxx

    Title3 [#450](https://github.com/realworldocaml/mdx/pull/450).
    xxx

    ### tarides/okra

    APPROVED Title4 [#166](https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361).
    xxx

    Title5 [#165](https://github.com/tarides/okra/issues/165).
    xxx |}]
