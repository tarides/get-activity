open Alcotest

module Msg = struct
  type 'a t = [ `Msg of 'a ]

  let pp f fs (`Msg s : 'a t) = Format.fprintf fs "%a" f s
  let eq f (`Msg s1 : 'a t) (`Msg s2 : 'a t) = f s1 s2

  let testable t =
    let pp = pp (Alcotest.pp t) in
    let eq = eq (Alcotest.equal t) in
    testable pp eq
end

let msg = Msg.testable
let string_msg = msg string
let or_msg x = result x string_msg

module Yojson = struct
  type t = Yojson.Safe.t

  let pp = Yojson.Safe.pp
  let eq = Yojson.Safe.equal
  let testable : t testable = testable pp eq
end

let yojson = Yojson.testable
