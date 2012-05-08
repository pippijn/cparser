open Ast

let of_list bts =
  match List.sort compare bts with
  | [BT_Char]					-> Char
  | [BT_Signed; BT_Char]			-> SChar
  | [BT_Unsigned; BT_Char]			-> UChar

  | [BT_Short]
  | [BT_Short; BT_Int]
  | [BT_Signed; BT_Short]
  | [BT_Signed; BT_Short; BT_Int]		-> SShort
  | [BT_Unsigned; BT_Short]
  | [BT_Unsigned; BT_Short; BT_Int]		-> UShort

  | [BT_Int]
  | [BT_Signed]
  | [BT_Signed; BT_Int]				-> SInt
  | [BT_Unsigned]
  | [BT_Unsigned; BT_Int]			-> UInt

  | [BT_Long]
  | [BT_Long; BT_Int]
  | [BT_Signed; BT_Long]
  | [BT_Signed; BT_Long; BT_Int]		-> SLong
  | [BT_Unsigned; BT_Long]
  | [BT_Unsigned; BT_Long; BT_Int]		-> ULong

  | [BT_Long; BT_Long]
  | [BT_Long; BT_Long; BT_Int]
  | [BT_Signed; BT_Long; BT_Long]
  | [BT_Signed; BT_Long; BT_Long; BT_Int]	-> SLongLong
  | [BT_Unsigned; BT_Long; BT_Long]
  | [BT_Unsigned; BT_Long; BT_Long; BT_Int]	-> ULongLong

  | [BT_IntN bits]
  | [BT_Signed; BT_IntN bits]			-> SIntN bits
  | [BT_Unsigned; BT_IntN bits]			-> UIntN bits

  | [BT_Bool]					-> Bool
  | [BT_Default]				-> SInt
  | [BT_Ellipsis]				-> Ellipsis
  | [BT_VaList]					-> VaList
  | [BT_Void]					-> Void
  | [BT_WCharT]					-> WCharT

  | [BT_FloatN bits]				-> FloatN bits
  | [BT_DecimalN bits]				-> DecimalN bits

  | [BT_Float]					-> Float
  | [BT_Double]					-> Double
  | [BT_Long; BT_Double]			-> LongDouble

  | bts -> die (Type_error ("invalid basic type", Some "6.7.2p2", [PartialBasicType bts]))


let to_list = function
  (* Integral types. *)
  | Bool	-> [BT_Bool]
  | Char	-> [BT_Char]
  | SChar	-> [BT_Signed; BT_Char]
  | UChar	-> [BT_Unsigned; BT_Char]
  | SShort	-> [BT_Short]
  | UShort	-> [BT_Unsigned; BT_Short]
  | SInt	-> [BT_Int]
  | UInt	-> [BT_Unsigned; BT_Int]
  | SLong	-> [BT_Long]
  | ULong	-> [BT_Unsigned; BT_Long]
  | SLongLong	-> [BT_Long; BT_Long]
  | ULongLong	-> [BT_Unsigned; BT_Long; BT_Long]

  (* Floating point types. *)
  | Float	-> [BT_Float]
  | Double	-> [BT_Double]
  | LongDouble	-> [BT_Long; BT_Double]

  (* Sized integral types. *)
  | SIntN bits	-> [BT_IntN bits]
  | UIntN bits	-> [BT_Unsigned; BT_IntN bits]

  (* Sized floating point types. *)
  | FloatN bits	-> [BT_FloatN bits]
  | DecimalN bits -> [BT_DecimalN bits]

  (* Other built-in types *)
  | VaList	-> [BT_VaList]
  | Ellipsis	-> [BT_Ellipsis]
  | Void	-> [BT_Void]
  | WCharT	-> [BT_WCharT]
