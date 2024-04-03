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
            timelineItems(last:10, itemTypes:[MERGED_EVENT]) {
              nodes {
                ... on MergedEvent {
                  createdAt
                  actor { login }
                }
              }
            }
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
  kind :
    [ `Issue
    | `PR
    | `Comment of [ `Issue | `PR ]
    | `Review of string
    | `Merge
    | `New_repo ];
  date : Datetime.t;
  url : string;
  title : string;
  body : string;
}

type t = { username : string; activity : item list Repo_map.t }

let read_issues =
  List.map (fun (c : Json.Issue.contribution) ->
      let date = c.occurredAt in
      let url = c.issue.url in
      let title = c.issue.title in
      let body = c.issue.body in
      let repo = c.issue.repository.nameWithOwner in
      { kind = `Issue; date; url; title; body; repo })

let read_issue_comments =
  List.map (fun (c : Json.comment) ->
      let date = c.publishedAt in
      let url = c.url in
      let title = c.issue.title in
      let kind =
        if Astring.String.is_infix ~affix:"/issues/" url then `Issue else `PR
      in
      let body = c.body in
      let repo = c.repository.nameWithOwner in
      { kind = `Comment kind; date; url; title; body; repo })

let read_prs ~username =
  List.fold_left
    (fun acc (c : Json.PullRequest.contribution) ->
      let date = c.occurredAt in
      let url = c.pullRequest.url in
      let title = c.pullRequest.title in
      let body = c.pullRequest.body in
      let repo = c.pullRequest.repository.nameWithOwner in
      let timeline_items = c.pullRequest.timelineItems.nodes in
      let acc = { kind = `PR; date; url; title; body; repo } :: acc in
      let acc =
        List.fold_left
          (fun acc (it : Json.PullRequest.timelineItem) ->
            let date = it.createdAt in
            let login = it.actor.login in
            if String.equal login username then
              { kind = `Merge; date; url; title; body = ""; repo } :: acc
            else acc)
          acc timeline_items
      in
      acc)
    []

let read_reviews =
  List.map (fun (c : Json.PullRequest.Review.contribution) ->
      let date = c.occurredAt in
      let state = c.pullRequestReview.state in
      let url = c.pullRequestReview.url in
      let title = c.pullRequestReview.pullRequest.title in
      let body = c.pullRequestReview.body in
      let repo = c.pullRequestReview.repository.nameWithOwner in
      { kind = `Review state; date; url; title; body; repo })

let read_repos =
  List.map (fun (c : Json.Repository.contribution) ->
      let date = c.occurredAt in
      let url = c.repository.url in
      let repo = c.repository.nameWithOwner in
      let title = "Created new repository" in
      { kind = `New_repo; date; url; title; body = ""; repo })

let of_json ~period:(from, to_) ~user json =
  let* json =
    match Json.t_of_yojson json with
    | x -> Ok x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure e, _)
      ->
        Error (`Msg e)
  in
  match json.data with
  | Some data ->
      let* root =
        match user with
        | User.Viewer ->
            data.viewer |> Option.to_result ~none:(`Msg "viewer field missing")
        | User.User _ ->
            data.user |> Option.to_result ~none:(`Msg "user field missing")
      in
      let username = root.login in
      let contribs = root.contributionsCollection in
      let items =
        let issues = read_issues contribs.issueContributions.nodes in
        let issue_comments = read_issue_comments root.issueComments.nodes in
        let prs = read_prs ~username contribs.pullRequestContributions.nodes in
        let reviews =
          read_reviews contribs.pullRequestReviewContributions.nodes
        in
        let repos = read_repos contribs.repositoryContributions.nodes in
        issues @ issue_comments @ prs @ reviews @ repos
      in
      let activity =
        (* GitHub seems to ignore the time part, so do the filtering here. *)
        items
        |> List.filter (fun item -> item.date >= from && item.date <= to_)
        |> List.fold_left
             (fun acc item ->
               let items =
                 Repo_map.find_opt item.repo acc |> Option.value ~default:[]
               in
               Repo_map.add item.repo (item :: items) acc)
             Repo_map.empty
      in
      Ok { username; activity }
  | None ->
      Fmt.error_msg "@[%a@]" (Fmt.list Fmt.string)
        (List.map (fun x -> x.Json.message) json.errors)

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
  | `Comment `Issue ->
      Fmt.pf f "Commented on issue %S [#%s](%s)" t.title (id t.url) t.url
  | `Comment `PR ->
      Fmt.pf f "Commented on PR %S [#%s](%s)" t.title (id t.url) t.url
  | `Review s -> Fmt.pf f "%s %s [#%s](%s)" s t.title (id t.url) t.url
  | `Merge -> Fmt.pf f "Merged %S [#%s](%s)" t.title (id t.url) t.url
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
