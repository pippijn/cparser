(** Module containing keyword tables and token strings. *)

val make_table : ('a * 'b) array -> ('a, 'b) Hashtbl.t
  (** Make a hash table from ar array of pairs. *)

val token_table : (string, C_tokens.token) Hashtbl.t
  (** Table containing C keywords. This table is used during normal parsing. *)
val attribute_table : (string, C_tokens.token) Hashtbl.t
  (** Table containing GCC attributes. This table is used while inside
      an [__attribute (())]. *)

val string_of_token : C_tokens.token -> string
  (** Get a valid C keyword, identifier or operator from a token. *)
val token_name : C_tokens.token -> string
  (** Get the ML token name. E.g. [token_name (KW_IDENTIFIER "id")] will
      return ["KW_IDENTIFIER"]. *)
val name_of_token : C_tokens.token -> string
  (** Get the token name including its data in ML constructor syntax.
      E.g. [token_name (KW_IDENTIFIER "id")] will return ["KW_IDENTIFIER \"id\""]. *)

val token_of_basic_type : Ast.partial_basic_type -> C_tokens.token
  (** Get the token for a partial basic type. There is no 1:1 mapping from
      [basic_type] to tokens, so to get the tokens for a [basic_type], one
      needs to deconstruct it into [partial_basic_type]s, first. *)
val string_of_basic_type : Ast.partial_basic_type -> string

val token_of_storage_class : Sclass.storage_class -> C_tokens.token
val string_of_storage_class : Sclass.storage_class -> string

val token_of_type_qualifier : Tqual.type_qualifier -> C_tokens.token
val string_of_type_qualifier : Tqual.type_qualifier -> string

val token_of_sue_kind : Ast.sue_kind -> C_tokens.token
val string_of_sue_kind : Ast.sue_kind -> string

val token_of_ternop : Ast.ternary_operator -> C_tokens.token * C_tokens.token
val string_of_ternop : Ast.ternary_operator -> string * string

val token_of_binop : Ast.binary_operator -> C_tokens.token
val string_of_binop : Ast.binary_operator -> string

val token_of_unop : Ast.unary_operator -> C_tokens.token
val string_of_unop : Ast.unary_operator -> string

val token_of_pseudoop : Ast.pseudo_operator -> C_tokens.token
val string_of_pseudoop : Ast.pseudo_operator -> string
