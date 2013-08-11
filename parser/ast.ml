open Sexplib.Conv

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
  with sexp

type attribute = expression Attributes.attribute

and annotations = expression Attributes.attributes_position_scope list
and position = Attributes.position list
and scope_and_position = Attributes.position_scope list
and scope = Attributes.scope list
and scope_and_attr = expression Attributes.attributes_scope list


(** {6 Statements} *)

and asm_argument =
  | AsmArgument of position * (*constraint*)string list * (*expr*)expression

and statement =
  | Nop
  (** Only a semicolon [;]. *)

  (* Statements *)
  | CompoundStatement of scope_and_position * (*body*)statement list
  (** [CompoundStatement (_, body)] *)
  | ExpressionStatement of position * (*expr*)expression option
  (** [ExpressionStatement (_, expr)] *)
  | DeclarationStatement of (*decl*)declaration
  (** [DeclarationStatement (decl)] *)

  (* Labelled statements *)
  | LabelledStatement of position * (*label*)string * (*stmt*)statement
  (** [LabelledStatement (_, label, stmt)] *)
  | LocalLabel of position * (*labels*)string list
  (** [LocalLabel (_, labels)] *)
  | CaseStatement of position * (*expr*)expression
  (** [CaseStatement (_, expr)] *)
  | DefaultStatement of position
  (** [DefaultStatement (_)] *)

  (* Selection statements *)
  | IfStatement of position * (*cond*)expression * (*then*)statement * (*else*)statement
  (** [IfStatement (_, cond, then, else)] *)
  | SwitchStatement of position * (*expr*)expression * (*cases*)statement
  (** [SwitchStatement (_, expr, cases)] *)

  (* Iteration statements *)
  | WhileStatement of position * (*cond*)expression * (*body*)statement
  (** [WhileStatement (_, cond, body)] *)
  | DoWhileStatement of position * (*body*)statement * (*cond*)expression
  (** [DoWhileStatement (_, body, cond)] *)
  | ForStatement of position * (*init*)expression option * (*cond*)expression option * (*next*)expression option * (*body*)statement
  (** [ForStatement (_, init, cond, next, body)] *)

  (* Jump statements *)
  | GotoStatement of position * (*label*)expression
  (** [GotoStatement (_, label)] *)
  | ContinueStatement of position
  (** [ContinueStatement (_)] *)
  | BreakStatement of position
  (** [BreakStatement (_)] *)
  | ReturnStatement of position * (*expr*)expression option
  (** [ReturnStatement (_, expr)] *)

  (* GCC asm statement *)
  | AsmStatement of position * (*volatile*)bool * (*code*)string list * (*in_regs*)asm_argument list * (*out_regs*)asm_argument list * (*clobber*)string list list * (*labels*)string list
  (** [AsmStatement (_, volatile, code, in_regs, out_regs, clobber, labels)] *)


(** {6 Expressions} *)

and expression =
  | WildcardExpr of position * string
  (** [WildcardExpr (_, wildcard)] *)

  | TypedExpression of (*ty*)ctype * (*value*)Constant.t * (*expr*)expression
  (** [TypedExpression (type, value, expr)] Expression with type information. *)

  | TernaryExpression of position * ternary_operator * (*cond*)expression * (*then*)expression option * (*else*)expression
  (** [TernaryExpression (_, op, cond, then, else)] *)
  | BinaryExpression of position * binary_operator * (*lhs*)expression * (*rhs*)expression
  (** [BinaryExpression (_, op, lhs, rhs)] *)
  | UnaryExpression of position * unary_operator * (*expr*)expression
  (** [UnaryExpression (_, op, expr)] *)
  | SizeofExpr of position * (*expr*)expression
  (** [SizeofExpr (_, expr)] *)
  | SizeofType of position * (*type*)ctype
  (** [SizeofType (_, type)] *)
  | AlignofExpr of position * (*expr*)expression
  (** [AlignofExpr (_, expr)] *)
  | AlignofType of position * (*type*)ctype
  (** [AlignofType (_, expr)] *)
  | Offsetof of position * (*type*)ctype * (*member*)expression
  (** [Offsetof (_, type, member)] *)
  | TypesCompatibleP of position * (*type1*)ctype * (*type2*)ctype
  (** [TypesCompatibleP (_, type1, type2)] *)
  | VaArg of position * (*ap*)expression * (*type*)ctype
  (** [VaArg (_, ap, type)] *)
  | FunctionCall of position * (*callee*)expression * (*args*)expression list
  (** [FunctionCall (_, callee, args)] *)
  | CompoundLiteral of position * (*type*)ctype * (*init*)expression
  (** [CompoundLiteral (_, type, inits)] *)
  | ArrayAccess of position * (*expr*)expression * (*index*)expression
  (** [ArrayAccess (_, expr, index)] *)
  | MemberAccess of position * (*expr*)expression * (*member*)string
  (** [MemberAccess (_, expr, member)] *)
  | PointerAccess of position * (*expr*)expression * (*member*)string
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
  | BraceExpression of position * (*stmt*)statement
  (** [BraceExpression (_, stmt)] *)

  (* Cast expression *)
  | Cast of position * (*type*)ctype * (*expr*)expression
  (** [Cast (_, type, expr)] *)

  (* Initialisers *)
  | InitialiserList of position * (*inits*)expression list
  (** [InitialiserList (_, inits)] *)
  | MemberDesignator of (*member*)string list
  (** [MemberDesignator (_, member)] *)
  | ArrayLabelledInitialiser of position * (*index*)expression * (*init*)expression
  (** [ArrayLabelledInitialiser (_, index, init)] *)
  | DesignatedInitialiser of position * (*designator*)expression * (*init*)expression
  (** [DesignatedInitialiser (_, designator, init)] *)


(** {6 Types} *)

and ctype =
  | NoType

  (* Wildcards *)
  | WildcardType of position * string
  (** [WildcardType (_, wildcard)] *)

  (* Types *)
  | PartialBasicType of partial_basic_type list
  (** [PartialBasicType (basics)] *)
  | BasicType of basic_type
  (** [BasicType (basic)] *)
  | QualifiedType of Tqual.type_qualifiers * (*unqual*)ctype
  (** [QualifiedType (tqs, unqual)] *)
  | PointerType of (*base*)ctype
  (** [PointerType (base)] *)
  | SUEType of annotations * sue_kind * (*tag*)string * (*members*)declaration list
  (** [SUEType (_, kind, tag, members)] *)
  | TypedefType of (*name*)string
  (** [TypedefType (name)] *)
  | ArrayType of (*arity*)expression option * (*base*)ctype
  (** [ArrayType (arity, base)] *)
  | FunctionType of (*rettype*)ctype * (*params*)declaration list
  (** [FunctionType (rettype, params)] *)
  | TypeofExpr of (*expr*)expression
  (** [TypeofExpr (expr)] *)
  | TypeofType of (*ty*)ctype
  (** [TypeofType (type)] *)


(** {6 Declarations} *)

and declaration =
  | NoDecl

  | TranslationUnit of (*decls*)declaration list
  (** [TranslationUnit (decls)] *)

  (* Wildcards *)
  | WildcardDecl of position * string
  (** [WildcardDecl (_, wildcard)] *)

  (* Syntax errors *)
  | SyntaxError of position * (*msg*)string * (*node*)declaration
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
  | FunctionDefinition of position * (*decl*)declaration * (*body*)statement
  (** [FunctionDefinition (_, decl, body)] *)
  | IdentifierDeclarator of annotations * (*id*)string
  (** [IdentifierDeclarator (_, id)] *)
  | StructDeclarator of position * (*decl*)declaration * (*bitfield*)expression option
  (** [StructDeclarator (_, decl, bitfield)] *)
  | TypedDecl of scope_and_position * Sclass.storage_classes * (*type*)ctype * (*untyped*)declaration * (*asm*)declaration * (*init*)expression option
  (** [TypedDecl (_, storage_classes, type, untyped_decl, asm, init)] *)
  | DeclaringList of position * (*decls*)declaration list
  (** [DeclaringList (_, decls)] *)

  (* Struct/union/enum types *)
  | Enumerator of position * (*id*)string * (*value*)expression option
  (** [Enumerator (_, id, value)] *)
  with sexp


(** {6 Main exception type} *)

type error =
  | Expression_error of string * string option * expression list
  | Statement_error of string * string option * statement list
  | Type_error of string * string option * ctype list
  | Declaration_error of string * string option * declaration list

  | Parse_error of string * declaration
  | Unimplemented of string

exception ASTError of error

let die error =
  raise (ASTError error)
