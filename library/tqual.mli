type type_qualifier =
  | TQ_Const
  | TQ_Volatile
  | TQ_Complex
  | TQ_Restrict

type type_qualifiers


val type_qualifiers_of_sexp : Sexplib.Sexp.t -> type_qualifiers
val sexp_of_type_qualifiers : type_qualifiers -> Sexplib.Sexp.t

val empty : type_qualifiers
val is_empty : type_qualifiers -> bool

val list_of : type_qualifiers -> type_qualifier list

val remove_compatible : type_qualifiers -> type_qualifiers
val add : type_qualifiers -> type_qualifier list -> type_qualifiers

val is_const    : type_qualifiers -> bool
val is_volatile : type_qualifiers -> bool
val is_complex  : type_qualifiers -> bool
val is_restrict : type_qualifiers -> bool
