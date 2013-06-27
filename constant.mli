type t =
  | NonConst
  | IntValue of Big_int.big_int
  | FloatValue of float
  | StringValue of string
  | WStringValue of ExUTF8.t

val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

val of_int : int -> t
val of_big_int : Big_int.big_int -> t
val of_float : float -> t
val of_string : string -> t
val of_wstring : string -> t

val to_int : t -> int
val to_big_int : t -> Big_int.big_int
val to_float : t -> float
val to_string : t -> string
val to_wstring : t -> string
