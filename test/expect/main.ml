open Get_activity

let%expect_test "Graphql.Request.make" =
  let request = Graphql.Request.make ~token:"" ~query:"" () in
  Fmt.pr "%a" Graphql.Request.pp request;
  [%expect
    {|
    {
      request =
        ((headers
      ((Authorization "bearer ") (host api.github.com)
       (user-agent ocaml-cohttp/v6.0.0_beta2)))
     (meth POST) (scheme (https)) (resource /graphql) (version HTTP_1_1)
     (encoding Unknown));
      body =
        <...>
      }
    |}]

let%expect_test "Contributions.request viewer" =
  let user = User.Viewer in
  let request = Contributions.request ~period:("", "") ~user ~token:"" in
  Fmt.pr "%a" Graphql.Request.pp request;
  [%expect
    {|
    {
      request =
        ((headers
      ((Authorization "bearer ") (host api.github.com)
       (user-agent ocaml-cohttp/v6.0.0_beta2)))
     (meth POST) (scheme (https)) (resource /graphql) (version HTTP_1_1)
     (encoding Unknown));
      body =
        <...>
      }
    |}]

let%expect_test "Contributions.request user" =
  let user = User.User "me" in
  let request = Contributions.request ~period:("", "") ~user ~token:"" in
  Fmt.pr "%a" Graphql.Request.pp request;
  [%expect
    {|
    {
      request =
        ((headers
      ((Authorization "bearer ") (host api.github.com)
       (user-agent ocaml-cohttp/v6.0.0_beta2)))
     (meth POST) (scheme (https)) (resource /graphql) (version HTTP_1_1)
     (encoding Unknown));
      body =
        <...>
      }
    |}]

let contributions_example ~user =
  let open Contributions in
  {
    username = user;
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

let%expect_test "Contributions.pp" =
  let open Contributions in
  let contributions_example = { username = "me"; activity = Repo_map.empty } in
  Format.printf "%a" pp contributions_example;
  [%expect {|
    (no activity) |}]

let%expect_test "Contributions.pp" =
  let open Contributions in
  Format.printf "%a" pp (contributions_example ~user:"me");
  [%expect
    {|
    ### gpetiot/config.ml

    Created repository [gpetiot/config.ml](https://github.com/gpetiot/config.ml).

    ### gpetiot/js_of_ocaml

    Created repository [gpetiot/js_of_ocaml](https://github.com/gpetiot/js_of_ocaml).

    ### ocaml-ppx/ocamlformat

    Represent the expr sequence as a list [#2533](https://github.com/ocaml-ppx/ocamlformat/pull/2533).
    xxx

    Merged "Represent the expr sequence as a list" [#2533](https://github.com/ocaml-ppx/ocamlformat/pull/2533).

    ### realworldocaml/mdx

    APPROVED Add upgrade instructions in the changelog for #446 [#449](https://github.com/realworldocaml/mdx/pull/449#pullrequestreview-1916654244).
    xxx

    Add an 'exec' label to execute include OCaml blocks [#450](https://github.com/realworldocaml/mdx/pull/450).
    xxx

    ### tarides/get-activity

    Add the PR/issues comments to the result of okra generate [#8](https://github.com/tarides/get-activity/issues/8).
    xxx

    ### tarides/okra

    APPROVED Make README.md more precise [#166](https://github.com/tarides/okra/pull/166#pullrequestreview-1905972361).
    xxx

    Commented on PR "Gitlab: exception when parsing Gitlab's JSON" [#114](https://github.com/tarides/okra/pull/114#issuecomment-1994130584).
    xxx

    Commented on issue "Gitlab: exception when parsing Gitlab's JSON" [#114](https://github.com/tarides/okra/issues/114#issuecomment-1994130584).
    xxx

    Make the `get-activity` package known to ocaml-ci [#165](https://github.com/tarides/okra/issues/165).
    xxx |}]
