open Get_activity

let test_one_week =
  let make_test name ~expected =
    let name = Printf.sprintf "one_week: %s" name in
    let test_fun () =
      let actual = Period.one_week in
      Alcotest.(check (float 0.)) name expected actual
    in
    (name, `Quick, test_fun)
  in
  [ make_test "default" ~expected:604800. ]

let test_to_8601 =
  let make_test name ~input ~expected =
    let name = Printf.sprintf "to_8601: %s" name in
    let test_fun () =
      let actual = Period.to_8601 input in
      Alcotest.(check string) name expected actual
    in
    (name, `Quick, test_fun)
  in
  [
    make_test "zero" ~input:0. ~expected:"1970-01-01T00:00:00Z";
    make_test "negative" ~input:(-1.) ~expected:"1969-12-31T23:59:59Z";
    make_test "ten" ~input:10. ~expected:"1970-01-01T00:00:10Z";
    make_test "one million" ~input:1_000_000. ~expected:"1970-01-12T13:46:40Z";
    make_test "one billion" ~input:1_000_000_000.
      ~expected:"2001-09-09T01:46:40Z";
  ]

let test_with_period = []
let suite = ("Period", test_one_week @ test_to_8601 @ test_with_period)
