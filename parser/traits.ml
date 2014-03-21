open Ast

let empty_position = [`Position Location.dummy]

let opt = Option.map


(* {{{ *)
let traits_of_expr = function
  (* Wildcards *)
  | WildcardExpr (trs, _)

  (* Expression *)
  | TernaryExpression (trs, _, _, _, _)
  | BinaryExpression (trs, _, _, _)
  | UnaryExpression (trs, _, _)
  | FunctionCall (trs, _, _)
  | ArrayAccess (trs, _, _)
  | MemberAccess (trs, _, _)
  | PointerAccess (trs, _, _)
  | SizeofExpr (trs, _)
  | SizeofType (trs, _)
  | AlignofExpr (trs, _)
  | AlignofType (trs, _)
  | Offsetof (trs, _, _)
  | TypesCompatibleP (trs, _, _)
  | VaArg (trs, _, _)

  (* Primary expression *)
  | Identifier (trs, _)
  | IntegerLiteral (trs, _, _, _)
  | FloatingLiteral (trs, _, _, _)
  | CharLiteral (trs, _, _)
  | StringLiteral (trs, _, _)
  | BraceExpression (trs, _)

  (* Cast expression *)
  | CompoundLiteral (trs, _, _)
  | Cast (trs, _, _)

  (* Initialisers *)
  | ArrayLabelledInitialiser (trs, _, _)
  | DesignatedInitialiser (trs, _, _)
  | InitialiserList (trs, _) -> trs

  (* Expression with type information *)
  | TypedExpression _

  | MemberDesignator _ -> empty_position


let traits_of_stmt = function
  | DeclarationStatement _
  | EmptyStmt -> empty_position

  (* Statements *)
  | CompoundStatement (trs, _) ->
      (* Ignore scope of CompoundStatement. *)
      Attributes.set_position (Attributes.position trs) []
  | ExpressionStatement (trs, _) -> trs

  (* Labelled statements *)
  | LabelledStatement (trs, _, _) -> trs
  | LocalLabel (trs, _) -> trs
  | CaseStatement (trs, _) -> trs
  | DefaultStatement (trs) -> trs

  (* Selection statements *)
  | IfStatement (trs, _, _, _) -> trs
  | SwitchStatement (trs, _, _) -> trs

  (* Iteration statements *)
  | WhileStatement (trs, _, _) -> trs
  | DoWhileStatement (trs, _, _) -> trs
  | ForStatement (trs, _, _, _, _) -> trs

  (* Jump statements *)
  | GotoStatement (trs, _) -> trs
  | ContinueStatement (trs) -> trs
  | BreakStatement (trs) -> trs
  | ReturnStatement (trs, _) -> trs

  (* GCC asm statement *)
  | AsmStatement (trs, _, _, _, _, _, _) -> trs


let traits_of_type = function
  | EmptyType -> empty_position

  (* Wildcards *)
  | WildcardType (trs, _) -> trs

  (* Types *)
  | PartialBasicType _
  | BasicType _
  | QualifiedType _
  | PointerType _
  | SUEType _
  | TypedefType _
  | ArrayType _
  | FunctionType _
  | TypeofType _
  | TypeofExpr _ -> empty_position


let traits_of_decl = function
  | EmptyDecl
  | TranslationUnit (_) -> []

  (* Wildcards *)
  | WildcardDecl (trs, _)

  (* #include etc. *)
  | PreprocessorDirective (trs, _)

  (* Syntax errors *)
  | SyntaxError (trs, _, _)

  (* Generic nodes *)
  | AsmSpecifier (trs, _)
  | FunctionDefinition (trs, _, _)
  | StructDeclarator (trs, _, _)
  | DeclaringList (trs, _)

  (* Statements *)
  | ToplevelAsm (trs, _)

  (* Struct/union/enum types *)
  | Enumerator (trs, _, _) -> trs

  | TypedDecl (trs, _, _, _, _, _) ->
      (* Ignore scope. *)
      begin match Attributes.position_opt trs with
      | None -> []
      | Some pos -> Attributes.set_position pos []
      end
  | IdentifierDeclarator (trs, _) ->
      (* Ignore attributes. *)
      begin match Attributes.position_opt trs with
      | None -> []
      | Some pos -> Attributes.set_position pos []
      end


let rec clear_deep_decl =
  let map = List.map in function
  | EmptyDecl -> EmptyDecl
  | TranslationUnit (decls) -> TranslationUnit (map clear_deep_decl decls)

  (* Wildcards *)
  | WildcardDecl (trs, wc) -> WildcardDecl ([], wc)

  (* #include etc. *)
  | PreprocessorDirective (trs, dir) -> PreprocessorDirective ([], dir)

  (* Syntax errors *)
  | SyntaxError (trs, msg, node) -> SyntaxError ([], msg, clear_deep_decl node)

  (* Statements *)
  | ToplevelAsm _ as node -> node

  (* Generic nodes *)
  | AsmSpecifier (trs, reg) -> AsmSpecifier ([], reg)
  | FunctionDefinition (trs, decl, body) -> FunctionDefinition ([], clear_deep_decl decl, clear_deep_stmt body)
  | IdentifierDeclarator (trs, id) -> IdentifierDeclarator ([], id)
  | StructDeclarator (trs, decl, bitfield) -> StructDeclarator ([], clear_deep_decl decl, opt clear_deep_expr bitfield)
  | TypedDecl (trs, sclasses, ty, untyped, asm, init) -> TypedDecl ([], sclasses, clear_deep_type ty, clear_deep_decl untyped, clear_deep_decl asm, opt clear_deep_expr init)
  | DeclaringList (trs, decls) -> DeclaringList ([], map clear_deep_decl decls)

  (* Struct/union/enum types *)
  | Enumerator (trs, id, value) -> Enumerator ([], id, opt clear_deep_expr value)


and clear_deep_expr =
  let map = List.map in function
  (* Wildcards *)
  | WildcardExpr (trs, wc) -> WildcardExpr ([], wc)

  (* Expression with type information *)
  | TypedExpression (ty, value, expr) -> TypedExpression (clear_deep_type ty, value, clear_deep_expr expr)

  (* Expression *)
  | TernaryExpression (trs, op, cond, then_expr, else_expr) -> TernaryExpression ([], op, clear_deep_expr cond, opt clear_deep_expr then_expr, clear_deep_expr else_expr)
  | BinaryExpression (trs, op, lhs, rhs) -> BinaryExpression ([], op, clear_deep_expr lhs, clear_deep_expr rhs)
  | UnaryExpression (trs, op, expr) -> UnaryExpression ([], op, clear_deep_expr expr)
  | Offsetof (trs, ty, member) -> Offsetof ([], clear_deep_type ty, clear_deep_expr member)
  | TypesCompatibleP (trs, ty1, ty2) -> TypesCompatibleP ([], clear_deep_type ty1, clear_deep_type ty2)
  | VaArg (trs, ap, ty) -> VaArg ([], clear_deep_expr ap, clear_deep_type ty)
  | FunctionCall (trs, expr, args) -> FunctionCall ([], clear_deep_expr expr, map clear_deep_expr args)
  | MemberAccess (trs, expr, member) -> MemberAccess ([], clear_deep_expr expr, member)
  | PointerAccess (trs, expr, member) -> PointerAccess ([], clear_deep_expr expr, member)
  | ArrayAccess (trs, expr, index) -> ArrayAccess ([], clear_deep_expr expr, clear_deep_expr index)
  | AlignofExpr (trs, expr) -> AlignofExpr ([], clear_deep_expr expr)
  | AlignofType (trs, ty) -> AlignofType ([], clear_deep_type ty)
  | SizeofExpr (trs, expr) -> SizeofExpr ([], clear_deep_expr expr)
  | SizeofType (trs, ty) -> SizeofType ([], clear_deep_type ty)
  | CompoundLiteral (trs, ty, init) -> CompoundLiteral ([], clear_deep_type ty, clear_deep_expr init)

  (* Primary expression *)
  | Identifier (trs, id) -> Identifier ([], id)
  | IntegerLiteral (trs, kind, lit, suffix) -> IntegerLiteral ([], kind, lit, suffix)
  | FloatingLiteral (trs, kind, lit, suffix) -> FloatingLiteral ([], kind, lit, suffix)
  | CharLiteral (trs, kind, lit) -> CharLiteral ([], kind, lit)
  | StringLiteral (trs, kind, lit) -> StringLiteral ([], kind, lit)
  | BraceExpression (trs, stmt) -> BraceExpression ([], clear_deep_stmt stmt)

  (* Cast expression *)
  | Cast (trs, ty, expr) -> Cast ([], clear_deep_type ty, clear_deep_expr expr)

  (* Initialisers *)
  | InitialiserList (trs, inits) -> InitialiserList ([], map clear_deep_expr inits)
  | MemberDesignator (members) as node -> node
  | ArrayLabelledInitialiser (trs, index, init) -> ArrayLabelledInitialiser ([], clear_deep_expr index, clear_deep_expr init)
  | DesignatedInitialiser (trs, designator, init) -> DesignatedInitialiser ([], clear_deep_expr designator, clear_deep_expr init)


and clear_deep_type =
  let map = List.map in function
  | EmptyType -> EmptyType

  (* Wildcards *)
  | WildcardType (trs, wc) -> WildcardType ([], wc)

  (* Types *)
  | PartialBasicType (bts) -> PartialBasicType (bts)
  | BasicType (bt) -> BasicType (bt)
  | QualifiedType (tquals, unqual) -> QualifiedType (tquals, clear_deep_type unqual)
  | PointerType (base) -> PointerType (clear_deep_type base)
  | SUEType (attrs, kind, tag, members) -> SUEType ([], kind, tag, map clear_deep_decl members)
  | TypedefType (id) -> TypedefType (id)
  | ArrayType (arity, base) -> ArrayType (opt clear_deep_expr arity, clear_deep_type base)
  | FunctionType (rettype, params) -> FunctionType (clear_deep_type rettype, map clear_deep_decl params)
  | TypeofType (ty) -> TypeofType (clear_deep_type ty)
  | TypeofExpr (expr) -> TypeofExpr (clear_deep_expr expr)


and clear_deep_stmt =
  let map = List.map in function
  | EmptyStmt -> EmptyStmt

  (* Statements *)
  | CompoundStatement (trs, stmts) -> CompoundStatement ([], map clear_deep_stmt stmts)
  | ExpressionStatement (trs, expr) -> ExpressionStatement ([], opt clear_deep_expr expr)
  | DeclarationStatement (decl) -> DeclarationStatement (clear_deep_decl decl)

  (* Labelled statements *)
  | LabelledStatement (trs, label, stmt) -> LabelledStatement ([], label, clear_deep_stmt stmt)
  | LocalLabel (trs, labels) -> LocalLabel ([], labels)
  | CaseStatement (trs, expr) -> CaseStatement ([], clear_deep_expr expr)
  | DefaultStatement (trs) -> DefaultStatement ([])

  (* Selection statements *)
  | IfStatement (trs, cond, then_stmt, else_stmt) -> IfStatement ([], clear_deep_expr cond, clear_deep_stmt then_stmt, clear_deep_stmt else_stmt)
  | SwitchStatement (trs, expr, cases) -> SwitchStatement ([], clear_deep_expr expr, clear_deep_stmt cases)

  (* Iteration statements *)
  | WhileStatement (trs, cond, body) -> WhileStatement ([], clear_deep_expr cond, clear_deep_stmt body)
  | DoWhileStatement (trs, body, cond) -> DoWhileStatement ([], clear_deep_stmt body, clear_deep_expr cond)
  | ForStatement (trs, init, cond, next, body) -> ForStatement ([], opt clear_deep_expr init, opt clear_deep_expr cond, opt clear_deep_expr next, clear_deep_stmt body)

  (* Jump statements *)
  | GotoStatement (trs, expr) -> GotoStatement ([], clear_deep_expr expr)
  | ContinueStatement (trs) -> ContinueStatement ([])
  | BreakStatement (trs) -> BreakStatement ([])
  | ReturnStatement (trs, expr) -> ReturnStatement ([], opt clear_deep_expr expr)

  (* GCC asm statement *)
  | AsmStatement (trs, volatile, code, in_args, out_args, clobber, labels) ->
      let clear (AsmArgument (trs, constr, expr)) =
        AsmArgument (trs, constr, clear_deep_expr expr)
      in
      AsmStatement ([], volatile, code, map clear in_args, map clear out_args, clobber, labels)
(* }}} *)


let copy_pos node =
  let trs = traits_of_decl node in
  Attributes.set_position (Attributes.position trs) []


let set_pos_decl pos = function
  | TypedDecl (trs, sclasses, ty, untyped, asm, init) ->
      TypedDecl (Attributes.set_position pos trs, sclasses, ty, untyped, asm, init)
  | node -> node


let pos_of_expr expr =
  try
    Attributes.position (traits_of_expr expr)
  with Option.No_value ->
    die (Expression_error ("no position", None, [expr]))


let pos_of_stmt stmt =
  try
    Attributes.position (traits_of_stmt stmt)
  with Option.No_value ->
    die (Statement_error ("no position", None, [stmt]))


let pos_of_type ty =
  try
    Attributes.position (traits_of_type ty)
  with Option.No_value ->
    die (Type_error ("no position", None, [ty]))


let pos_of_decl decl =
  Option.default Location.dummy (Attributes.position_opt (traits_of_decl decl))


let rec add_attrs atts decl =
  match atts with
  | [] -> decl
  | attrs ->
      match decl with
      | EmptyDecl ->
          IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) [], "")
      | IdentifierDeclarator (trs, id) ->
          IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) trs, id)
      | StructDeclarator (trs, decl, bitfield) ->
          StructDeclarator (trs, add_attrs attrs decl, bitfield)
      | TypedDecl (trs, sc, ty, decl, asm, init) ->
          TypedDecl (trs, sc, ty, add_attrs attrs decl, asm, init)
      | DeclaringList (trs, decl :: tl) ->
          DeclaringList (trs, add_attrs attrs decl :: tl)
      | FunctionDefinition (trs, decl, body) ->
          FunctionDefinition (trs, add_attrs attrs decl, body)
      | decl -> die (Declaration_error ("invalid declaration", None, [decl]))
