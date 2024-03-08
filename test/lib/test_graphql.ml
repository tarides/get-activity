open Get_activity

let test_request =
  let make_test name ?variables ~token ~query ~expected () =
    let name = Printf.sprintf "request: %s" name in
    let test_fun () =
      let actual = Graphql.request ?variables ~token ~query () in
      Alcotest.(check Alcotest_ext.request) name expected actual
    in
    (name, `Quick, test_fun)
  in
  [
    make_test "no token" ~token:"" ~query:""
      ~expected:
        {
          meth = `POST;
          url = "https://api.github.com/graphql";
          headers = [ ("Authorization", "bearer ") ];
          body = `Assoc [ ("query", `String "") ];
        }
      ();
  ]

let suite = ("Graphql", test_request)
