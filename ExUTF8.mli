type t = private string

val sexp_of_t : t -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t

val uchar_of_int : int -> string
val adopt : string -> t
val length : t -> int
