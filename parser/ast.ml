(** The C abstract syntax definition. *)

(** {6 Basic types during construction in parser} *)

type partial_basic_type =
  | BT_Default		(** default-int *)

  (* Signedness *)
  | BT_Signed		(** [signed] *)
  | BT_Unsigned		(** [unsigned] *)

  (* Integer modifiers "short int", "long int", "long long int" *)
  | BT_Short		(** [short] *)
  | BT_Long		(** [long] *)

  (* int and sized integral types *)
  | BT_Int		(** [int] *)
  | BT_IntN of int	(** [__intNN] *)

  (* Other integral types *)
  | BT_Bool		(** [_Bool] *)
  | BT_Char		(** [char] *)
  | BT_WCharT		(** [wchar_t] *)

  (* Floating point types *)
  | BT_Float		(** [float] *)
  | BT_Double		(** [double] *)

  (* Sized floating point types *)
  | BT_FloatN of int	(** [__floatNN] *)
  | BT_DecimalN of int	(** [_DecimalNN] *)

  (* Other built-in types *)
  | BT_VaList		(** [__builtin_va_list] *)
  | BT_Ellipsis		(** [...] *)
  | BT_Void		(** [void] *)


(** {6 Basic types after normalisation} *)

and basic_type =
  (* Integral types. *)
  | Bool		(** [_Bool] *)
  | Char		(** [char] *)
  | SChar		(** [signed char] *)
  | UChar		(** [unsigned char] *)
  | SShort		(** [signed short] *)
  | UShort		(** [unsigned short] *)
  | SInt		(** [signed int] *)
  | UInt		(** [unsigned int] *)
  | SLong		(** [signed long] *)
  | ULong		(** [unsigned long] *)
  | SLongLong		(** [signed long long] *)
  | ULongLong		(** [unsigned long long] *)

  (* Floating point types. *)
  | Float		(** [float] *)
  | Double		(** [double] *)
  | LongDouble		(** [long double] *)

  (* Sized integral types. *)
  | SIntN of int	(** [signed __intNN] *)
  | UIntN of int	(** [unsigned __intNN] *)

  (* Sized floating point types. *)
  | FloatN of int	(** [__floatNN] *)
  | DecimalN of int	(** [_DecimalNN] *)

  (* Other built-in types *)
  | VaList		(** [__builtin_va_list] *)
  | Ellipsis		(** [...] *)
  | Void		(** [void] *)
  | WCharT		(** [wchar_t] *)


(** {6 User defined type tag kinds} *)

and sue_kind =
  | SUE_Struct		(** [struct] *)
  | SUE_Union		(** [union] *)
  | SUE_Enum		(** [enum] *)


(** {6 Numeric literal kinds} *)

and integer_literal_kind =
  (* Integer *)
  | LIT_Bin		(** [0b01011] *)
  | LIT_Dec		(** [12345] *)
  | LIT_Hex		(** [0xdeadbeef] *)
  | LIT_Oct		(** [0755] *)


and floating_literal_kind =
  (* Floating point *)
  | LIT_Float		(** [1.0e+24] *)
  | LIT_HexFloat	(** [0x1.0p-500] *)


(** {6 Character literal kinds} *)

and char_literal_kind =
  (* Character *)
  | LIT_Char		(** ['c'] *)
  | LIT_WChar		(** [L'c'] *)


(** {6 String literal kinds} *)

and string_literal_kind =
  | LIT_String		(** ["string"] *)
  | LIT_WString		(** [L"string"] *)


(** {6 Ternary operators} *)

and ternary_operator =
  (* Conditional expressions *)
  | OP_Conditional	(** [a ? b : c] *)


(** {6 Binary operators} *)

and binary_operator =
  (* Assignment expressions *)
  | OP_Assign		(** "[a = b]" *)
  | OP_MultiplyAssign	(** "[a *= b]" *)
  | OP_DivideAssign	(** "[a /= b]" *)
  | OP_ModuloAssign	(** "[a %= b]" *)
  | OP_AddAssign	(** "[a += b]" *)
  | OP_SubtractAssign	(** "[a -= b]" *)
  | OP_ShiftLeftAssign	(** "[a <<= b]" *)
  | OP_ShiftRightAssign	(** "[a >>= b]" *)
  | OP_BitwiseAndAssign	(** "[a &= b]" *)
  | OP_BitwiseXorAssign	(** "[a ^= b]" *)
  | OP_BitwiseOrAssign	(** "[a |= b]" *)

  (* Binary expressions *)
  | OP_Multiply		(** "[a * b]" *)
  | OP_Divide		(** "[a / b]" *)
  | OP_Modulo		(** "[a % b]" *)
  | OP_Add		(** "[a + b]" *)
  | OP_Subtract		(** "[a - b]" *)
  | OP_ShiftLeft	(** "[a << b]" *)
  | OP_ShiftRight	(** "[a >> b]" *)
  | OP_Less		(** "[a < b]" *)
  | OP_Greater		(** "[a > b]" *)
  | OP_LessEqual	(** "[a <= b]" *)
  | OP_GreaterEqual	(** "[a >= b]" *)
  | OP_Equal		(** "[a == b]" *)
  | OP_NotEqual		(** "[a != b]" *)
  | OP_BitwiseAnd	(** "[a & b]" *)
  | OP_BitwiseXor	(** "[a ^ b]" *)
  | OP_BitwiseOr	(** "[a | b]" *)
  | OP_LogicalAnd	(** "[a && b]" *)
  | OP_LogicalOr	(** "[a || b]" *)
  | OP_Ellipsis		(** "[a ... b]" *)
  | OP_Comma		(** "[a , b]" *)


(** {6 Unary operators} *)

and unary_operator =
  (* Unary expressions *)
  | OP_PostIncrement	(** "[a++]" *)
  | OP_PostDecrement	(** "[a--]" *)
  | OP_PreIncrement	(** "[++a]" *)
  | OP_PreDecrement	(** "[--a]" *)
  | OP_AddressOf	(** "[&a]" *)
  | OP_AddressOfLabel	(** "[&&a]" *)
  | OP_Dereference	(** "[*a]" *)
  | OP_Identity		(** "[+a]" *)
  | OP_Negate		(** "[-a]" *)
  | OP_BitwiseNot	(** "[~a]" *)
  | OP_LogicalNot	(** "[!a]" *)
  | OP_Imag		(** "[imag a]" *)
  | OP_Real		(** "[real a]" *)


(** {6 Pseudo operators} *)

and pseudo_operator =
  | OP_HighestPrecedence	(** Pseudo-operator with a precedence higher than all other operators. *)
  | OP_LowestPrecedence		(** Pseudo-operator with a precedence lower than all other operators. *)
  | OP_FunctionCall		(** "[a (b)]" *)
  | OP_ArrayAccess		(** "[a[b]]" *)
  | OP_MemberAccess		(** "[a.b]" *)
  | OP_PointerAccess		(** "[a->b]" *)
  | OP_DesignatedInitialiser	(** "[.a = b]" *)
  | OP_ArrayLabelledInitialiser	(** "[[a] = b]" *)
  | OP_Cast			(** "[(a)b]" *)
  | OP_Sizeof			(** "[sizeof (a)]" *)
  | OP_Alignof			(** "[__alignof (a)]" *)
  deriving (Show)

type attribute = expr Attributes.attribute

and annotations = expr Attributes.attributes_position_scope list
and position = Attributes.position list
and scope_and_position = Attributes.position_scope list


(** {6 Statements} *)

and asm_argument =
  | AsmArgument of position * (*constraint*)string list * (*expr*)expr

and stmt = {
  s : stmt_;
  s_sloc : Location.t;
}

and stmt_ =
  | EmptyStmt
  (** Only a semicolon [;]. *)

  (* Statements *)
  | CompoundStatement of (*scope*)string * (*body*)stmt list
  | ExpressionStatement of (*expr*)expr option
  | DeclarationStatement of (*decl*)decl

  (* Labelled statements *)
  | LabelledStatement of (*label*)string * (*stmt*)stmt
  | LocalLabel of (*labels*)string list
  | CaseStatement of (*expr*)expr
  | DefaultStatement

  (* Selection statements *)
  | IfStatement of (*cond*)expr * (*then*)stmt * (*else*)stmt
  | SwitchStatement of (*expr*)expr * (*cases*)stmt

  (* Iteration statements *)
  | WhileStatement of (*cond*)expr * (*body*)stmt
  | DoWhileStatement of (*body*)stmt * (*cond*)expr
  | ForStatement of (*init*)expr option * (*cond*)expr option * (*next*)expr option * (*body*)stmt

  (* Jump statements *)
  | GotoStatement of (*label*)expr
  | ContinueStatement
  | BreakStatement
  | ReturnStatement of (*expr*)expr option

  (* GCC asm statement *)
  | AsmStatement of (*volatile*)bool * (*code*)string list * (*in_regs*)asm_argument list * (*out_regs*)asm_argument list * (*clobber*)string list list * (*labels*)string list


(** {6 Expressions} *)

and expr =
  | WildcardExpr of position * string
  (** [WildcardExpr (_, wildcard)] *)

  | TypedExpression of (*ty*)ctyp * (*value*)Constant.t * (*expr*)expr
  (** [TypedExpression (type, value, expr)] expr with type information. *)

  | TernaryExpression of position * ternary_operator * (*cond*)expr * (*then*)expr option * (*else*)expr
  (** [TernaryExpression (_, op, cond, then, else)] *)
  | BinaryExpression of position * binary_operator * (*lhs*)expr * (*rhs*)expr
  (** [BinaryExpression (_, op, lhs, rhs)] *)
  | UnaryExpression of position * unary_operator * (*expr*)expr
  (** [UnaryExpression (_, op, expr)] *)
  | SizeofExpr of position * (*expr*)expr
  (** [SizeofExpr (_, expr)] *)
  | SizeofType of position * (*type*)ctyp
  (** [SizeofType (_, type)] *)
  | AlignofExpr of position * (*expr*)expr
  (** [AlignofExpr (_, expr)] *)
  | AlignofType of position * (*type*)ctyp
  (** [AlignofType (_, expr)] *)
  | Offsetof of position * (*type*)ctyp * (*member*)expr
  (** [Offsetof (_, type, member)] *)
  | TypesCompatibleP of position * (*type1*)ctyp * (*type2*)ctyp
  (** [TypesCompatibleP (_, type1, type2)] *)
  | VaArg of position * (*ap*)expr * (*type*)ctyp
  (** [VaArg (_, ap, type)] *)
  | FunctionCall of position * (*callee*)expr * (*args*)expr list
  (** [FunctionCall (_, callee, args)] *)
  | CompoundLiteral of position * (*type*)ctyp * (*init*)expr
  (** [CompoundLiteral (_, type, inits)] *)
  | ArrayAccess of position * (*expr*)expr * (*index*)expr
  (** [ArrayAccess (_, expr, index)] *)
  | MemberAccess of position * (*expr*)expr * (*member*)string
  (** [MemberAccess (_, expr, member)] *)
  | PointerAccess of position * (*expr*)expr * (*member*)string
  (** [PointerAccess (_, expr, member)] *)

  (* Primary expression *)
  | Identifier of position * (*id*)string
  (** [Identifier (_, id)] *)
  | IntegerLiteral of position * integer_literal_kind * string * string option
  (** [IntegerLiteral (_, kind, number, suffix)] *)
  | FloatingLiteral of position * floating_literal_kind * string * string option
  (** [FloatingLiteral (_, kind, number, suffix)] *)
  | CharLiteral of position * char_literal_kind * string
  (** [CharLiteral (_, kind, character)] *)
  | StringLiteral of position * string_literal_kind * string list
  (** [StringLiteral (_, kind, strings)] *)
  | BraceExpression of position * (*stmt*)stmt
  (** [BraceExpression (_, stmt)] *)

  (* Cast expression *)
  | Cast of position * (*type*)ctyp * (*expr*)expr
  (** [Cast (_, type, expr)] *)

  (* Initialisers *)
  | InitialiserList of position * (*inits*)expr list
  (** [InitialiserList (_, inits)] *)
  | MemberDesignator of (*member*)string list
  (** [MemberDesignator (_, member)] *)
  | ArrayLabelledInitialiser of position * (*index*)expr * (*init*)expr
  (** [ArrayLabelledInitialiser (_, index, init)] *)
  | DesignatedInitialiser of position * (*designator*)expr * (*init*)expr
  (** [DesignatedInitialiser (_, designator, init)] *)


(** {6 Types} *)

and ctyp =
  | EmptyType

  (* Wildcards *)
  | WildcardType of position * string
  (** [WildcardType (_, wildcard)] *)

  (* Types *)
  | PartialBasicType of partial_basic_type list
  (** [PartialBasicType (basics)] *)
  | BasicType of basic_type
  (** [BasicType (basic)] *)
  | QualifiedType of Tqual.type_qualifiers * (*unqual*)ctyp
  (** [QualifiedType (tqs, unqual)] *)
  | PointerType of (*base*)ctyp
  (** [PointerType (base)] *)
  | SUEType of annotations * sue_kind * (*tag*)string * (*members*)decl list
  (** [SUEType (_, kind, tag, members)] *)
  | TypedefType of (*name*)string
  (** [TypedefType (name)] *)
  | ArrayType of (*arity*)expr option * (*base*)ctyp
  (** [ArrayType (arity, base)] *)
  | FunctionType of (*rettype*)ctyp * (*params*)decl list
  (** [FunctionType (rettype, params)] *)
  | TypeofExpr of (*expr*)expr
  (** [TypeofExpr (expr)] *)
  | TypeofType of (*ty*)ctyp
  (** [TypeofType (type)] *)


(** {6 Declarations} *)

and decl =
  | EmptyDecl

  | TranslationUnit of (*decls*)decl list
  (** [TranslationUnit (decls)] *)

  (* Wildcards *)
  | WildcardDecl of position * string
  (** [WildcardDecl (_, wildcard)] *)

  (* Syntax errors *)
  | SyntaxError of position * (*msg*)string * (*node*)decl
  (** [SyntaxError (_, msg, node)] *)

  (* #include etc. *)
  | PreprocessorDirective of position * string
  (** [PreprocessorDirective (_, directive)] *)

  (* Toplevel __asm__ *)
  | ToplevelAsm of position * (*code*)string list
  (** [ToplevelAsm (_, code)] *)

  (* Declarations *)
  | AsmSpecifier of position * (*register*)string list
  (** [AsmSpecifier (_, register)] *)
  | FunctionDefinition of position * (*decl*)decl * (*body*)stmt
  (** [FunctionDefinition (_, decl, body)] *)
  | IdentifierDeclarator of annotations * (*id*)string
  (** [IdentifierDeclarator (_, id)] *)
  | StructDeclarator of position * (*decl*)decl * (*bitfield*)expr option
  (** [StructDeclarator (_, decl, bitfield)] *)
  | TypedDecl of scope_and_position * Sclass.storage_classes * (*type*)ctyp * (*untyped*)decl * (*asm*)decl * (*init*)expr option
  (** [TypedDecl (_, storage_classes, type, untyped_decl, asm, init)] *)
  | DeclaringList of position * (*decls*)decl list
  (** [DeclaringList (_, decls)] *)

  (* Struct/union/enum types *)
  | Enumerator of position * (*id*)string * (*value*)expr option
  (** [Enumerator (_, id, value)] *)
  deriving (Show)


(** {6 Main exception type} *)

type error =
  | Expression_error of string * string option * expr list
  | Statement_error of string * string option * stmt list
  | Type_error of string * string option * ctyp list
  | Declaration_error of string * string option * decl list

  | Parse_error of string * decl
  | Unimplemented of string

exception ASTError of error

let die error =
  raise (ASTError error)
