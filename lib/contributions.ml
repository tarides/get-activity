module Json = Contributions_json_response

let ( let* ) = Result.bind

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
            repository { nameWithOwner }
          }
        }
      }
      repositoryContributions(first: 100) {
        nodes {
          occurredAt
          repository { url nameWithOwner }
        }
      }
    }
    issueComments(last: 40) {
      nodes {
        url
        publishedAt
        repository { nameWithOwner }
        issue { title }
        body
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
end

module Repo_map = Map.Make (String)

type item = {
  repo : string;
  kind : [ `Issue | `Issue_comment | `PR | `Review of string | `New_repo ];
  date : Datetime.t;
  url : string;
  title : string;
  body : string;
}

type t = { username : string; activity : item list Repo_map.t }

let read_issues =
  List.map (fun (c : Json.issueContribution) ->
      let date = c.occurredAt in
      let url = c.issue.url in
      let title = c.issue.title in
      let body = c.issue.body in
      let repo = c.issue.repository.nameWithOwner in
      { kind = `Issue; date; url; title; body; repo })

let read_issue_comments =
  List.map (fun (c : Json.issueComment) ->
      let date = c.publishedAt in
      let url = c.url in
      let title = c.issue.title in
      let body = c.body in
      let repo = c.repository.nameWithOwner in
      { kind = `Issue_comment; date; url; title; body; repo })

let read_prs =
  List.map (fun (c : Json.pullRequestContribution) ->
      let date = c.occurredAt in
      let url = c.pullRequest.url in
      let title = c.pullRequest.title in
      let body = c.pullRequest.body in
      let repo = c.pullRequest.repository.nameWithOwner in
      { kind = `PR; date; url; title; body; repo })

let read_reviews =
  List.map (fun (c : Json.pullRequestReviewContribution) ->
      let date = c.occurredAt in
      let state = c.pullRequestReview.state in
      let url = c.pullRequestReview.url in
      let title = c.pullRequestReview.pullRequest.title in
      let body = c.pullRequestReview.body in
      let repo = c.pullRequestReview.repository.nameWithOwner in
      { kind = `Review state; date; url; title; body; repo })

let read_repos =
  List.map (fun (c : Json.repositoryContribution) ->
      let date = c.occurredAt in
      let url = c.repository.url in
      let repo = c.repository.nameWithOwner in
      let title = "Created new repository" in
      { kind = `New_repo; date; url; title; body = ""; repo })

let of_json ~from ~user json =
  let* json =
    match Json.t_of_yojson json with
    | x -> Ok x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        Error (`Msg (Printexc.to_string exn))
  in
  let* root =
    match user with
    | User.Viewer ->
        json.data.viewer |> Option.to_result ~none:(`Msg "viewer field missing")
    | User.User _ ->
        json.data.user |> Option.to_result ~none:(`Msg "user field missing")
  in
  let username = root.login in
  let contribs = root.contributionsCollection in
  let items =
    let issues = read_issues contribs.issueContributions.nodes in
    let issue_comments = read_issue_comments root.issueComments.nodes in
    let prs = read_prs contribs.pullRequestContributions.nodes in
    let reviews = read_reviews contribs.pullRequestReviewContributions.nodes in
    let repos = read_repos contribs.repositoryContributions.nodes in
    issues @ issue_comments @ prs @ reviews @ repos
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
  Ok { username; activity }

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
  | `Issue_comment ->
      Fmt.pf f "Commented on %S [#%s](%s)" t.title (id t.url) t.url
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
             {
               repo = "tarides/okra";
               kind = `Issue_comment;
               date = "2024-03-13T11:09:56Z";
               url =
                 "https://github.com/tarides/okra/issues/114#issuecomment-1994130584";
               title = "Title6";
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
    xxx

    Commented on "Title6" [#114](https://github.com/tarides/okra/issues/114#issuecomment-1994130584).
    xxx |}]
