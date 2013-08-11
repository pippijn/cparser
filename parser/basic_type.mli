(** Operations for constructing and deconstructing basic types. *)

val of_list : Ast.partial_basic_type list -> Ast.basic_type
  (** Construct a [basic_type] from a list of [partial_basic_type]. Only
      certain combinations of [partial_basic_type] are allowed. The list order
      is irrelevant.

      @see "6.2.5" Types *)
val to_list : Ast.basic_type -> Ast.partial_basic_type list
  (** Deconstruct a [basic_type] into a list of [partial_basic_type]. The
      resulting list can safely be passed to [of_list] to reconstruct the
      [basic_type] passed. In other words,

      @see "6.2.5" Types *)
