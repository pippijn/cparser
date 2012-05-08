type t = Lexing.position * Lexing.position

val sexp_of_t : t -> Sexplib.Sexp.t
val t_of_sexp : Sexplib.Sexp.t -> t

val dummy : t
