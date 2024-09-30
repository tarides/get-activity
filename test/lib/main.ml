let () =
  Alcotest.run "get-activity-lib"
    [ Test_token.suite; Test_period.suite; Test_contributions.suite ]
