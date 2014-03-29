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

and expr = {
  e : expr_;
  e_sloc : Location.t;
}

and expr_ =
  | WildcardExpr of string

  | TypedExpression of (*ty*)ctyp * (*value*)Constant.t * (*expr*)expr

  | TernaryExpression of ternary_operator * (*cond*)expr * (*then*)expr option * (*else*)expr
  | BinaryExpression of binary_operator * (*lhs*)expr * (*rhs*)expr
  | UnaryExpression of unary_operator * (*expr*)expr
  | SizeofExpr of (*expr*)expr
  | SizeofType of (*type*)ctyp
  | AlignofExpr of (*expr*)expr
  | AlignofType of (*type*)ctyp
  | Offsetof of (*type*)ctyp * (*member*)expr
  | TypesCompatibleP of (*type1*)ctyp * (*type2*)ctyp
  | VaArg of (*ap*)expr * (*type*)ctyp
  | FunctionCall of (*callee*)expr * (*args*)expr list
  | CompoundLiteral of (*type*)ctyp * (*init*)expr
  | ArrayAccess of (*expr*)expr * (*index*)expr
  | MemberAccess of (*expr*)expr * (*member*)string
  | PointerAccess of (*expr*)expr * (*member*)string

  (* Primary expression *)
  | Identifier of (*id*)string
  | IntegerLiteral of integer_literal_kind * string * string option
  | FloatingLiteral of floating_literal_kind * string * string option
  | CharLiteral of char_literal_kind * string
  | StringLiteral of string_literal_kind * string list
  | BraceExpression of (*stmt*)stmt

  (* Cast expression *)
  | Cast of (*type*)ctyp * (*expr*)expr

  (* Initialisers *)
  | InitialiserList of (*inits*)expr list
  | ArrayLabelledInitialiser of (*index*)expr * (*init*)expr
  | DesignatedInitialiser of (*designator*)desg * (*init*)expr

and desg = {
  dg : string list;
  dg_sloc : Location.t;
}


(** {6 Types} *)

and ctyp =
  | EmptyType

  (* Wildcards *)
  | WildcardType of position * string

  (* Types *)
  | PartialBasicType of partial_basic_type list
  | BasicType of basic_type
  | QualifiedType of Tqual.type_qualifiers * (*unqual*)ctyp
  | PointerType of (*base*)ctyp
  | SUEType of annotations * sue_kind * (*tag*)string * (*members*)decl list
  | TypedefType of (*name*)string
  | ArrayType of (*arity*)expr option * (*base*)ctyp
  | FunctionType of (*rettype*)ctyp * (*params*)decl list
  | TypeofExpr of (*expr*)expr
  | TypeofType of (*ty*)ctyp


(** {6 Declarations} *)

and decl =
  | EmptyDecl

  | TranslationUnit of (*decls*)decl list

  (* Wildcards *)
  | WildcardDecl of position * string

  (* Syntax errors *)
  | SyntaxError of position * (*msg*)string * (*node*)decl

  (* #include etc. *)
  | PreprocessorDirective of position * string

  (* Toplevel __asm__ *)
  | ToplevelAsm of position * (*code*)string list

  (* Declarations *)
  | AsmSpecifier of position * (*register*)string list
  | FunctionDefinition of position * (*decl*)decl * (*body*)stmt
  | IdentifierDeclarator of annotations * (*id*)string
  | StructDeclarator of position * (*decl*)decl * (*bitfield*)expr option
  | TypedDecl of scope_and_position * Sclass.storage_classes * (*type*)ctyp * (*untyped*)decl * (*asm*)decl * (*init*)expr option
  | DeclaringList of position * (*decls*)decl list

  (* Struct/union/enum types *)
  | Enumerator of position * (*id*)string * (*value*)expr option
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
