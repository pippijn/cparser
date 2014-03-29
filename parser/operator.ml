open Ast


type operator =
  | TernaryOperator of ternary_operator
  | BinaryOperator of binary_operator
  | UnaryOperator of unary_operator
  | PseudoOperator of pseudo_operator


(* cf. man 7 operator *)
let precedence_of_ternop = function
  | OP_Conditional -> 12


let precedence_of_binop = function
  | OP_Multiply
  | OP_Divide
  | OP_Modulo -> 3

  | OP_Add
  | OP_Subtract -> 4

  | OP_ShiftLeft
  | OP_ShiftRight -> 5

  | OP_Less
  | OP_Greater
  | OP_LessEqual
  | OP_GreaterEqual -> 6

  | OP_Equal
  | OP_NotEqual -> 7

  | OP_BitwiseAnd -> 8
  | OP_BitwiseXor -> 9
  | OP_BitwiseOr -> 10
  | OP_LogicalAnd -> 11
  | OP_LogicalOr -> 12

  | OP_MultiplyAssign
  | OP_DivideAssign
  | OP_ModuloAssign
  | OP_AddAssign
  | OP_SubtractAssign
  | OP_ShiftLeftAssign
  | OP_ShiftRightAssign
  | OP_BitwiseAndAssign
  | OP_BitwiseXorAssign
  | OP_BitwiseOrAssign
  | OP_Assign -> 14

  | OP_Ellipsis
  | OP_Comma -> 15


let precedence_of_unop = function
  (* all unary operators have the same precedence *)
  | op -> 2


let precedence_of_pseudoop = function
  (* special negative precedence says there is
   * no need to bracket anything in this expression *)
  | OP_HighestPrecedence
  | OP_DesignatedInitialiser
  | OP_ArrayLabelledInitialiser -> -1

  | OP_FunctionCall
  | OP_MemberAccess
  | OP_PointerAccess
  | OP_ArrayAccess -> 1

  | OP_Cast
  | OP_Alignof
  | OP_Sizeof -> 2

  | OP_LowestPrecedence -> 16 (* lower than OP_Comma *)



let precedence_of_operator = function
  | TernaryOperator op -> precedence_of_ternop op
  | BinaryOperator op -> precedence_of_binop op
  | UnaryOperator op -> precedence_of_unop op
  | PseudoOperator op -> precedence_of_pseudoop op


let lowest = PseudoOperator OP_LowestPrecedence
let highest = PseudoOperator OP_HighestPrecedence


let rec wrap expr =
  match expr.e with
  | TernaryExpression (op, _, _, _) -> TernaryOperator op
  | BinaryExpression (op, _, _) -> BinaryOperator op
  | UnaryExpression (op, _) -> UnaryOperator op
  | SizeofExpr _
  | SizeofType _ -> PseudoOperator OP_Sizeof
  | AlignofExpr _
  | AlignofType _ -> PseudoOperator OP_Alignof
  | Cast _ -> PseudoOperator OP_Cast
  | FunctionCall _ -> PseudoOperator OP_FunctionCall
  | MemberAccess _ -> PseudoOperator OP_MemberAccess
  | ArrayAccess _ -> PseudoOperator OP_ArrayAccess
  | PointerAccess _ -> PseudoOperator OP_PointerAccess

  (* Initialisers *)
  | InitialiserList _

  | Identifier _
  | IntegerLiteral _
  | FloatingLiteral _
  | CharLiteral _
  | StringLiteral _
  | BraceExpression _
  | CompoundLiteral _
  | Offsetof _
  | TypesCompatibleP _
  | VaArg _ -> highest

  | ArrayLabelledInitialiser _ -> PseudoOperator OP_ArrayLabelledInitialiser
  | DesignatedInitialiser _ -> PseudoOperator OP_DesignatedInitialiser

  | WildcardExpr _ -> failwith "wildcard expression has no operator"


type associativity =
  | Left
  | Right
  | Nonassoc


let associativity = function
  | TernaryOperator op ->
      begin match op with
      | OP_Conditional -> Right
      end
  | BinaryOperator op ->
      begin match op with
      | OP_Add
      | OP_BitwiseAnd
      | OP_BitwiseOr
      | OP_BitwiseXor
      | OP_Comma
      | OP_Divide
      | OP_Equal
      | OP_Greater
      | OP_GreaterEqual
      | OP_Less
      | OP_LessEqual
      | OP_LogicalAnd
      | OP_LogicalOr
      | OP_Modulo
      | OP_Multiply
      | OP_NotEqual
      | OP_ShiftLeft
      | OP_ShiftRight
      | OP_Subtract -> Left
      | OP_MultiplyAssign
      | OP_DivideAssign
      | OP_ModuloAssign
      | OP_AddAssign
      | OP_SubtractAssign
      | OP_ShiftLeftAssign
      | OP_ShiftRightAssign
      | OP_BitwiseAndAssign
      | OP_BitwiseXorAssign
      | OP_BitwiseOrAssign
      | OP_Assign -> Right
      | OP_Ellipsis -> Nonassoc
      end
  | UnaryOperator op ->
      begin match op with
      (* all unary operators are right-to-left associative *)
      | _ -> Right
      end
  | PseudoOperator op ->
      begin match op with
      | OP_ArrayAccess
      | OP_FunctionCall
      | OP_MemberAccess
      | OP_PointerAccess -> Left
      | OP_Alignof
      | OP_Cast
      | OP_Sizeof -> Right
      | OP_ArrayLabelledInitialiser -> Nonassoc
      | OP_DesignatedInitialiser -> Nonassoc
      | OP_HighestPrecedence -> Nonassoc
      | OP_LowestPrecedence -> Nonassoc
      end


let is_associative = function
  | BinaryOperator op ->
      begin match op with
      | OP_Add
      | OP_BitwiseAnd
      | OP_BitwiseOr
      | OP_BitwiseXor
      | OP_Comma
      | OP_LogicalAnd
      | OP_LogicalOr
      | OP_Multiply
      | OP_Subtract -> true
      | OP_AddAssign
      | OP_Assign
      | OP_BitwiseAndAssign
      | OP_BitwiseOrAssign
      | OP_BitwiseXorAssign
      | OP_Divide
      | OP_DivideAssign
      | OP_Ellipsis
      | OP_Equal
      | OP_Greater
      | OP_GreaterEqual
      | OP_Less
      | OP_LessEqual
      | OP_Modulo
      | OP_ModuloAssign
      | OP_MultiplyAssign
      | OP_NotEqual
      | OP_ShiftLeft
      | OP_ShiftLeftAssign
      | OP_ShiftRight
      | OP_ShiftRightAssign
      | OP_SubtractAssign -> false
      end
  | PseudoOperator OP_ArrayAccess -> true
  | _ -> false


let is_postfix = function
  | OP_PostIncrement
  | OP_PostDecrement -> true
  | _ -> false

let is_prefix op = not (is_postfix op)


let is_assignment = function
  | OP_MultiplyAssign
  | OP_DivideAssign
  | OP_ModuloAssign
  | OP_AddAssign
  | OP_SubtractAssign
  | OP_ShiftLeftAssign
  | OP_ShiftRightAssign
  | OP_BitwiseAndAssign
  | OP_BitwiseXorAssign
  | OP_BitwiseOrAssign
  | OP_Assign -> true
  | _ -> false


let assignment_base = function
  | OP_MultiplyAssign
  | OP_DivideAssign
  | OP_ModuloAssign
  | OP_AddAssign
  | OP_SubtractAssign
  | OP_ShiftLeftAssign
  | OP_ShiftRightAssign
  | OP_BitwiseAndAssign
  | OP_BitwiseXorAssign
  | OP_BitwiseOrAssign
  | OP_Assign -> true
  | op -> failwith ("operator " ^ (Token.string_of_binop op) ^ " is not an assignment operator")


let is_incdec = function
  | OP_PostIncrement
  | OP_PostDecrement
  | OP_PreIncrement
  | OP_PreDecrement -> true
  | _ -> false


let is_comparison = function
  | OP_Equal
  | OP_NotEqual
  | OP_Less
  | OP_LessEqual
  | OP_Greater
  | OP_GreaterEqual -> true
  | _ -> false


let is_arithmetic = function
  | OP_Add
  | OP_Subtract
  | OP_Multiply
  | OP_Divide
  | OP_Modulo
  | OP_BitwiseAnd
  | OP_BitwiseXor
  | OP_BitwiseOr
  | OP_LogicalAnd
  | OP_LogicalOr
  | OP_ShiftLeft
  | OP_ShiftRight -> true
  | _ -> false
