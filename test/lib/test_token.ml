open Get_activity

let test_load =
  let make_test name ~input ~expected =
    let name = Printf.sprintf "load: %s" name in
    let test_fun () =
      let actual = Token.load input in
      Alcotest.(check (Alcotest_ext.or_msg string)) name expected actual
    in
    (name, `Quick, test_fun)
  in
  let error_test name path =
    let msg =
      Format.sprintf
        {|Can't open GitHub token file (%s: No such file or directory).
Go to https://github.com/settings/tokens to generate one.|}
        path
    in
    make_test name ~input:path ~expected:(Error (`Msg msg))
  in
  [
    error_test "empty" ""; error_test "invalid path" "invalid-path/invalid-path";
  ]

let suite = ("Token", test_load)
