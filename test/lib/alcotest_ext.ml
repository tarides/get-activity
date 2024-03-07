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

module Curly = struct
  module Meth = struct
    type t = Curly.Meth.t

    let pp = Curly.Meth.pp

    let eq (x : t) (y : t) =
      let x = Format.asprintf "%a" Curly.Meth.pp x in
      let y = Format.asprintf "%a" Curly.Meth.pp y in
      String.equal x y
  end

  module Header = struct
    type t = Curly.Header.t

    let pp = Curly.Header.pp

    let eq (x : t) (y : t) =
      let x = Format.asprintf "%a" Curly.Header.pp x in
      let y = Format.asprintf "%a" Curly.Header.pp y in
      String.equal x y
  end
end

module Request = struct
  type t = Get_activity.Graphql.request

  let pp fs (x : t) =
    Format.fprintf fs
      "@[<hv 2>{@;\
       meth = %a;@;\
       url = %S@;\
       headers =@ %a@;\
       body =@ @[<hv 0>%a@];@]@;\
       }"
      Curly.Meth.pp x.meth x.url Curly.Header.pp x.headers Yojson.pp x.body

  let eq (x : t) (y : t) =
    Curly.Meth.eq x.meth y.meth
    && String.equal x.url y.url
    && Curly.Header.eq x.headers y.headers
    && Yojson.eq x.body y.body

  let testable = Alcotest.testable pp eq
end

let request = Request.testable
