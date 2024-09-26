open Get_activity

module Testable = struct
  module Datetime = struct
    let pp fs x = Format.fprintf fs "%S" x
    let eq = String.equal
  end

  module Item = struct
    module Kind = struct
      type t =
        [ `Issue
        | `PR
        | `Comment of [ `Issue | `PR ]
        | `Review of string
        | `Merge
        | `New_repo ]

      let pp fs = function
        | `Issue -> Format.fprintf fs "`Issue"
        | `PR -> Format.fprintf fs "`PR"
        | `Comment `Issue -> Format.fprintf fs "`Comment `Issue"
        | `Comment `PR -> Format.fprintf fs "`Comment `PR"
        | `Review x -> Format.fprintf fs "`Review %S" x
        | `Merge -> Format.fprintf fs "`Merge"
        | `New_repo -> Format.fprintf fs "`New_repo"

      let eq (x : t) (y : t) =
        match (x, y) with
        | `Issue, `Issue
        | `PR, `PR
        | `Comment `Issue, `Comment `Issue
        | `Comment `PR, `Comment `PR
        | `Merge, `Merge
        | `New_repo, `New_repo ->
            true
        | `Review x, `Review y -> String.equal x y
        | _ -> false
    end

    type t = Contributions.item

    let pp fs (x : t) =
      Format.fprintf fs
        "@[<hv 2>{@;\
         repo = %S;@;\
         kind = %a;@;\
         date = %a;@;\
         url = %S;@;\
         title = %S;@;\
         body = %S;@]@;\
         }@,"
        x.repo Kind.pp x.kind Datetime.pp x.date x.url x.title x.body

    let eq (x : t) (y : t) =
      String.equal x.repo y.repo && Kind.eq x.kind y.kind
      && Datetime.eq x.date y.date && String.equal x.url y.url
      && String.equal x.title y.title
      && String.equal x.body y.body
  end

  module Repo_map = struct
    type 'a t = 'a Contributions.Repo_map.t

    let pp f fs (x : 'a t) =
      Contributions.Repo_map.iter
        (fun key v ->
          Format.fprintf fs
            "@[<hv 2>{@;key = %S;@;value =@ @[<hv 0>%a@];@]@;}@," key f v)
        x

    let eq = Contributions.Repo_map.equal
  end

  module Contributions = struct
    type t = Contributions.t

    let pp fs (x : t) =
      Format.fprintf fs
        "@[<hv 2>{@;username = %S;@;activity =@ @[<hv 0>%a@];@]@;}" x.username
        (Repo_map.pp (Format.pp_print_list Item.pp))
        x.activity

    (* [List.equal] requires OCaml >= 4.12 *)
    let list_equal eq lx ly =
      try
        List.iter2 (fun x y -> if not (eq x y) then failwith "not equal") lx ly;
        true
      with _ -> false

    let eq (x : t) (y : t) =
      String.equal x.username y.username
      && Repo_map.eq (list_equal Item.eq) x.activity y.activity

    let testable = Alcotest.testable pp eq
  end

  let contributions = Contributions.testable
end

let request ~user =
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

let or_viewer = function User.User u -> u | Viewer -> "gpetiot"

let activity_example ~user =
  Format.sprintf
    {|
{
  "data": {
    %S: {
      "login": %S,
      "contributionsCollection": {
        "issueContributions": {
          "nodes": [
            {
              "occurredAt": "2024-03-04T11:55:37Z",
              "issue": {
                "url": "https://github.com/tarides/get-activity/issues/8",
                "title": "Add the PR/issues comments to the result of okra generate",
                "body": "xxx",
                "repository": {
                  "nameWithOwner": "tarides/get-activity"
                }
              }
            },
            {
              "occurredAt": "2024-02-27T12:05:04Z",
              "issue": {
                "url": "https://github.com/tarides/okra/issues/165",
                "title": "Make the `get-activity` package known to ocaml-ci",
                "body": "xxx",
                "repository": {
                  "nameWithOwner": "tarides/okra"
                }
              }
            }
          ]
        },
        "pullRequestContributions": {
          "nodes": [
            {
              "occurredAt": "2024-03-05T11:21:22Z",
              "pullRequest": {
                "url": "https://github.com/ocaml-ppx/ocamlformat/pull/2533",
                "title": "Represent the expr sequence as a list",
                "body": "xxx",
                "repository": {
                  "nameWithOwner": "ocaml-ppx/ocamlformat"
                },
                "timelineItems": {
                  "nodes": [
                    {
                      "createdAt": "2024-03-13T11:09:56Z",
                      "actor": {
                        "login": %S
                      }
                    }
                  ]
                }
              }
            },
            {
              "occurredAt": "2024-03-04T17:20:11Z",
              "pullRequest": {
                "url": "https://github.com/realworldocaml/mdx/pull/450",
                "title": "Add an 'exec' label to execute include OCaml blocks",
                "body": "xxx",
                "repository": {
                  "nameWithOwner": "realworldocaml/mdx"
                },
                "timelineItems": {
                  "nodes": [
                    {
                      "createdAt": "2024-04-02T20:22:37Z",
                      "actor": {
                        "login": "xxx"
                      }
                    }
                  ]
                }
              }
            },
            null
          ]
        },
        "pullRequestReviewContributions": {
          "nodes": [
            {
              "occurredAt": "2024-03-05T11:43:04Z",
              "pullRequestReview": {
                "url": "https://github.com/realworldocaml/mdx/pull/449#pullrequestreview-1916654244",
                "pullRequest": {
                  "title": "Add upgrade instructions in the changelog for #446"
                },
                "body": "xxx",
                "state": "APPROVED",
                "repository": {
                  "nameWithOwner": "realworldocaml/mdx"
                }
              }
            },
            {
              "occurredAt": "2024-02-28T11:09:41Z",
              "pullRequestReview": {
                "url": "https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361",
                "pullRequest": {
                  "title": "Make README.md more precise"
                },
                "body": "xxx",
                "state": "APPROVED",
                "repository": {
                  "nameWithOwner": "tarides/okra"
                }
              }
            }
          ]
        },
        "repositoryContributions": {
          "nodes": [
            {
              "occurredAt": "2024-03-02T09:40:41Z",
              "repository": {
                "url": "https://github.com/gpetiot/config.ml",
                "nameWithOwner": "gpetiot/config.ml"
              }
            },
            {
              "occurredAt": "2024-03-01T10:43:33Z",
              "repository": {
                "url": "https://github.com/gpetiot/js_of_ocaml",
                "nameWithOwner": "gpetiot/js_of_ocaml"
              }
            }
          ]
        }
      },
      "issueComments": {
        "nodes": [
          {
            "url": "https://github.com/tarides/okra/issues/114#issuecomment-1994130584",
            "publishedAt": "2024-03-13T11:09:56Z",
            "issue": {
              "title": "Gitlab: exception when parsing Gitlab's JSON"
            },
            "repository": {
              "nameWithOwner": "tarides/okra"
            },
            "body": "xxx"
          },
          {
            "url": "https://github.com/tarides/okra/pull/114#issuecomment-1994130584",
            "publishedAt": "2024-03-13T11:09:56Z",
            "issue": {
              "title": "Gitlab: exception when parsing Gitlab's JSON"
            },
            "repository": {
              "nameWithOwner": "tarides/okra"
            },
            "body": "xxx"
          }
        ]
      }
    }
  }
}
|}
    (User.response_field user) (user |> or_viewer) (user |> or_viewer)

let activity_example_json ~user =
  Yojson.Safe.from_string (activity_example ~user)

let error_example =
  {|
  {
  "errors": [
    {
      "path": [
        "query",
        "viewer",
        "contributionsCollection",
        "to"
      ],
      "extensions": {
        "code": "argumentLiteralsIncompatible",
        "typeName": "Field",
        "argumentName": "to"
      },
      "locations": [
        {
          "line": 4,
          "column": 5
        }
      ],
      "message": "Error."
    }
  ]
} |}

let error_example_json = Yojson.Safe.from_string error_example

let contributions_example1 ~user =
  let open Contributions in
  {
    username = user |> or_viewer;
    activity =
      Repo_map.of_seq @@ List.to_seq
      @@ [
           ( "tarides/okra",
             [
               {
                 repo = "tarides/okra";
                 kind = `Review "APPROVED";
                 date = "2024-02-28T11:09:41Z";
                 url =
                   "https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361";
                 title = "Make README.md more precise";
                 body = "xxx";
               };
               {
                 repo = "tarides/okra";
                 kind = `Issue;
                 date = "2024-02-27T12:05:04Z";
                 url = "https://github.com/tarides/okra/issues/165";
                 title = "Make the `get-activity` package known to ocaml-ci";
                 body = "xxx";
               };
             ] );
         ];
  }

let contributions_example2 ~user =
  let open Contributions in
  {
    username = user |> or_viewer;
    activity =
      Repo_map.of_seq @@ List.to_seq
      @@ [
           ( "gpetiot/config.ml",
             [
               {
                 repo = "gpetiot/config.ml";
                 kind = `New_repo;
                 date = "2024-03-02T09:40:41Z";
                 url = "https://github.com/gpetiot/config.ml";
                 title = "Created new repository";
                 body = "";
               };
             ] );
           ( "gpetiot/js_of_ocaml",
             [
               {
                 repo = "gpetiot/js_of_ocaml";
                 kind = `New_repo;
                 date = "2024-03-01T10:43:33Z";
                 url = "https://github.com/gpetiot/js_of_ocaml";
                 title = "Created new repository";
                 body = "";
               };
             ] );
           ( "ocaml-ppx/ocamlformat",
             [
               {
                 repo = "ocaml-ppx/ocamlformat";
                 kind = `PR;
                 date = "2024-03-05T11:21:22Z";
                 url = "https://github.com/ocaml-ppx/ocamlformat/pull/2533";
                 title = "Represent the expr sequence as a list";
                 body = "xxx";
               };
               {
                 repo = "ocaml-ppx/ocamlformat";
                 kind = `Merge;
                 date = "2024-03-13T11:09:56Z";
                 url = "https://github.com/ocaml-ppx/ocamlformat/pull/2533";
                 title = "Represent the expr sequence as a list";
                 body = "";
               };
             ] );
           ( "realworldocaml/mdx",
             [
               {
                 repo = "realworldocaml/mdx";
                 kind = `Review "APPROVED";
                 date = "2024-03-05T11:43:04Z";
                 url =
                   "https://github.com/realworldocaml/mdx/pull/449#pullrequestreview-1916654244";
                 title = "Add upgrade instructions in the changelog for #446";
                 body = "xxx";
               };
               {
                 repo = "realworldocaml/mdx";
                 kind = `PR;
                 date = "2024-03-04T17:20:11Z";
                 url = "https://github.com/realworldocaml/mdx/pull/450";
                 title = "Add an 'exec' label to execute include OCaml blocks";
                 body = "xxx";
               };
             ] );
           ( "tarides/get-activity",
             [
               {
                 repo = "tarides/get-activity";
                 kind = `Issue;
                 date = "2024-03-04T11:55:37Z";
                 url = "https://github.com/tarides/get-activity/issues/8";
                 title =
                   "Add the PR/issues comments to the result of okra generate";
                 body = "xxx";
               };
             ] );
           ( "tarides/okra",
             [
               {
                 repo = "tarides/okra";
                 kind = `Review "APPROVED";
                 date = "2024-02-28T11:09:41Z";
                 url =
                   "https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361";
                 title = "Make README.md more precise";
                 body = "xxx";
               };
               {
                 repo = "tarides/okra";
                 kind = `Comment `PR;
                 date = "2024-03-13T11:09:56Z";
                 url =
                   "https://github.com/tarides/okra/pull/114#issuecomment-1994130584";
                 title = "Gitlab: exception when parsing Gitlab's JSON";
                 body = "xxx";
               };
               {
                 repo = "tarides/okra";
                 kind = `Comment `Issue;
                 date = "2024-03-13T11:09:56Z";
                 url =
                   "https://github.com/tarides/okra/issues/114#issuecomment-1994130584";
                 title = "Gitlab: exception when parsing Gitlab's JSON";
                 body = "xxx";
               };
               {
                 repo = "tarides/okra";
                 kind = `Issue;
                 date = "2024-02-27T12:05:04Z";
                 url = "https://github.com/tarides/okra/issues/165";
                 title = "Make the `get-activity` package known to ocaml-ci";
                 body = "xxx";
               };
             ] );
         ];
  }

let test_of_json =
  let make_test name ~period ~user json ~expected =
    let name = Printf.sprintf "of_json: %s" name in
    let test_fun () =
      let actual = Contributions.of_json ~period ~user json in
      Alcotest.(check (Alcotest_ext.or_msg Testable.contributions))
        name expected actual
    in
    (name, `Quick, test_fun)
  in
  [
    (let user = User.Viewer in
     make_test "no token"
       ~period:("2024-02-27T12:05:04Z", "2024-02-28T11:09:41Z")
       ~user
       (activity_example_json ~user)
       ~expected:(Ok (contributions_example1 ~user)));
    (let user = User.User "gpetiot" in
     make_test "no token"
       ~period:("2024-02-27T12:05:04Z", "2024-03-13T11:09:56Z")
       ~user
       (activity_example_json ~user)
       ~expected:(Ok (contributions_example2 ~user)));
    (let user = User.User "gpetiot" in
     make_test "no token" ~period:("", "") ~user
       (activity_example_json ~user)
       ~expected:
         (Ok
            {
              username = user |> or_viewer;
              activity = Contributions.Repo_map.empty;
            }));
    (let user = User.User "gpetiot" in
     make_test "errors"
       ~period:("2024-02-27T12:05:04Z", "2024-03-13T11:09:56Z")
       ~user error_example_json ~expected:(Fmt.error_msg "Error."));
  ]

let test_is_empty =
  let make_test name ~input ~expected =
    let name = Printf.sprintf "is_empty: %s" name in
    let test_fun () =
      let actual = Contributions.is_empty input in
      Alcotest.(check bool) name expected actual
    in
    (name, `Quick, test_fun)
  in
  [
    make_test "empty"
      ~input:
        { Contributions.username = ""; activity = Contributions.Repo_map.empty }
      ~expected:true;
    make_test "not empty"
      ~input:(contributions_example1 ~user:Viewer)
      ~expected:false;
  ]

let suite = ("Contributions", test_of_json @ test_is_empty)
