open Big_int
open Sexplib.Sexp
open Sexplib.Conv

type ustring = ExUTF8.t with sexp

type t =
  | NonConst
  | IntValue of big_int
  | FloatValue of float
  | StringValue of string
  | WStringValue of ustring
  with sexp


let of_big_int i = IntValue i
let of_int i = of_big_int (big_int_of_int i)
let of_float f = FloatValue f
let of_string s = StringValue s
let of_wstring s = WStringValue (ExUTF8.adopt s)

let to_big_int = function IntValue v -> v | _ -> failwith "to_int"
let to_int v = int_of_big_int (to_big_int v)
let to_float = function FloatValue v -> v | _ -> failwith "to_float"
let to_string = function StringValue v -> v | _ -> failwith "to_string"
let to_wstring = function WStringValue v -> (v :> string) | _ -> failwith "to_wstring"
