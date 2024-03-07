val msg : 'a Alcotest.testable -> [ `Msg of 'a ] Alcotest.testable
val string_msg : [ `Msg of string ] Alcotest.testable

val or_msg :
  'a Alcotest.testable -> ('a, [ `Msg of string ]) result Alcotest.testable

val yojson : Yojson.Safe.t Alcotest.testable
val request : Get_activity.Graphql.request Alcotest.testable
