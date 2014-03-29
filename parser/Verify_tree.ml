open Ast


type verify_context =
  | TopLevel
  | FormalParm
  | StructField
  | Other


let bad_expr msg node =
  die (Expression_error (msg, None, [node]))

let bad_stmt msg node =
  die (Statement_error (msg, None, [node]))

let bad_type msg node =
  die (Type_error (msg, None, [node]))

let bad_decl msg node =
  die (Declaration_error (msg, None, [node]))


let is_decl n =
  if Predicates.is_decl n then []
  else bad_decl "node is not a declaration" n


let opt f = Option.map_default f []


let forall f l =
  List.fold_left (fun x a -> x @ f a) [] l


let rec verify_node c node =
  match node.d with
  | DeclaringList (decls) -> forall (verify_node c) decls
  | PreprocessorDirective (dir) -> []

  (*** STATEMENTS ***)
  | ToplevelAsm _ -> []

  (*** STRUCT/UNION/ENUM ***)

  | Enumerator (name, expr) ->
      opt verify_expr expr


  (*** DECLS ***)

  (* declarations *)
  | TypedDecl (_, sclasses, declty, decl, { d = EmptyDecl }, None)
    when Sclass.is_empty sclasses ->
      verify_type declty @
      opt_verify_declr c decl
  | TypedDecl (_, sclasses, declty, decl, asm, init) ->
      (if c = StructField then
         bad_decl "bad declaration form in struct member" node
       else
         []
      ) @
      verify_sc sclasses @
      verify_type declty @
      opt_verify_declr c decl @
      opt_verify_asmdq asm @
      opt verify_expr init

  | FunctionDefinition (ty, body) ->
      (* type of function definitions is a function type *)
      let rec verify ty =
        match ty.d with
        | TypedDecl (_, _, ({ t = FunctionType _ } as ty), _, _, _) ->
            verify_type ty
        | _ -> bad_decl "function definition does not have function type" ty
      in
      verify ty @
      verify_stmt body

  | _ -> die (Parse_error ("unexpected node", node))


and verify_declr c node =
  match node.d with
  | DeclaringList (decls) ->
      forall (verify_declr c) decls
  | StructDeclarator (decl, bitfield) ->
      (if c <> StructField then
         bad_decl "found struct declarator outside struct" node
       else
         []
      ) @
      verify_decl c decl @
      opt verify_expr bitfield
  | WildcardDecl _
  | IdentifierDeclarator _ -> []
  | _ -> die (Parse_error ("unexpected declarator", node))


and opt_verify_asmdq = function
  | { d = EmptyDecl } -> []
  | n -> verify_asmdq n

and opt_verify_declr c = function
  | { d = EmptyDecl } -> []
  | n -> verify_declr c n

and opt_verify_stmt = function
  | { s = EmptyStmt } -> []
  | n -> verify_stmt n


and verify_stmt stmt =
  match stmt.s with
  (*** STATEMENTS ***)
  | CompoundStatement (_, stmts) ->
      forall verify_stmt stmts
  | ExpressionStatement (expr) ->
      opt verify_expr expr
  | DeclarationStatement (decl) ->
      verify_decl Other decl
  | IfStatement (cond, then_stmt, else_stmt) ->
      verify_expr cond @
      verify_stmt then_stmt @
      opt_verify_stmt else_stmt
  | SwitchStatement (cond, body)
  | WhileStatement (cond, body)
  | DoWhileStatement (body, cond) ->
      verify_stmt body @
      verify_expr cond
  | ForStatement (init, cond, next, body) ->
      forall (opt verify_expr) [init; cond; next] @
      verify_stmt body
  | CaseStatement (expr) ->
      verify_expr expr
  | DefaultStatement
  | BreakStatement
  | ContinueStatement ->
      []
  | ReturnStatement (expr) ->
      opt verify_expr expr
  | LabelledStatement ("", _) ->
      bad_stmt "empty label" stmt
  | GotoStatement (label) ->
      verify_expr label
  | LabelledStatement (label, stmt) ->
      opt_verify_stmt stmt
  | LocalLabel (labels) ->
      forall (function "" -> bad_stmt "empty label" stmt | _ -> []) labels
  | AsmStatement (volatile, code, in_regs, out_regs, clobber, labels) ->
      let verify (AsmArgument (constr, expr)) = verify_expr expr in
      forall verify in_regs @
      forall verify out_regs

  | _ -> bad_stmt "verify_stmt" stmt


and verify_expr expr =
  begin match expr.e_type with
    | { t = EmptyType } -> []
    | ty -> verify_type ty
  end @

  match expr.e with
  | WildcardExpr ("") -> bad_expr "empty wildcard" expr
  | WildcardExpr _ -> []

  | Identifier ("") -> bad_expr "empty identifier" expr
  | Identifier (_) -> []

  | IntegerLiteral (_, "", _)
  | FloatingLiteral (_, "", _) -> bad_expr "empty literal" expr
  | IntegerLiteral (_, _, Some "") -> bad_expr "empty suffix on literal" expr
  | FloatingLiteral (_, _, Some "") -> bad_expr "empty suffix on literal" expr
  | IntegerLiteral (_, _, _) -> []
  | FloatingLiteral (_, _, _) -> []

  | CharLiteral (kind, "") -> bad_expr "empty char literal" expr
  | CharLiteral (kind, _) -> []

  | StringLiteral (kind, strs) ->
      forall (function "" -> bad_expr "empty string literal" expr | _ -> []) strs

  | InitialiserList (inits) ->
      forall verify_expr inits

  | TernaryExpression (_, cond, then_expr, else_expr) ->
      verify_expr cond @
      opt verify_expr then_expr @
      verify_expr else_expr
  | DesignatedInitialiser (members, init) ->
      forall
        (function "" -> bad_expr "empty designator" expr | _ -> [])
        members.dg
      @ verify_expr init
  | ArrayLabelledInitialiser (left, right)
  | BinaryExpression (_, left, right) ->
      forall verify_expr [left; right]
  | UnaryExpression (_, expr) ->
      verify_expr expr

  | CompoundLiteral (ty, init) ->
      verify_type ty @
      verify_expr init

  | BraceExpression (stmt) ->
      verify_stmt stmt

  | ArrayAccess (expr, index) ->
      verify_expr expr @
      verify_expr index

  | PointerAccess (expr, member)
  | MemberAccess (expr, member) ->
      verify_expr expr

  | AlignofType (ty)
  | SizeofType (ty) ->
      verify_type ty

  | AlignofExpr (expr)
  | SizeofExpr (expr) ->
      verify_expr expr

  | Cast (ty, expr) ->
      verify_type ty @
      verify_expr expr

  | FunctionCall (callee, args) ->
      verify_expr callee @
      forall verify_expr args

  | Offsetof (ty, member) ->
      verify_type ty @
      verify_expr member

  | VaArg (expr, ty) ->
      verify_expr expr @
      verify_type ty

  | TypesCompatibleP (ty1, ty2) ->
      verify_type ty1 @
      verify_type ty2


and verify_type node =
  match node.t with
  | EmptyType -> bad_type "empty type" node

  | WildcardType _ -> []

  (*** TYPES ***)

  | PartialBasicType [] -> bad_type "empty basic type" node
  | PartialBasicType _ -> []

  | BasicType _ -> []

  | FunctionType (retty, params) ->
      verify_type retty @
      forall (verify_decl FormalParm) params

  | PointerType (base) ->
      verify_type base

  | QualifiedType (tqs, base) ->
      verify_tq tqs @
      verify_type base

  | TypedefType "" -> bad_type "empty typedef type name" node
  | TypedefType name -> []

  | TypeofType (ty) ->
      verify_type ty
  | TypeofExpr (expr) ->
      verify_expr expr

  | ArrayType (arity, base) ->
      opt verify_expr arity @
      verify_type base

  (*** STRUCT/UNION/ENUM ***)

  (* allow empty structs and unions *)
  | SUEType (_, _, _, [{ d = EmptyDecl }]) -> []

  (* struct/union members *)
  | SUEType (_, (SUE_Union | SUE_Struct), _, members) ->
      forall (opt_verify_declr StructField) members

  (* enums are never empty *)
  | SUEType (_, SUE_Enum, _, members) ->
      forall (verify_node StructField) members




and verify_decl c decl =
  verify_node c decl @
  match c with
  | TopLevel
  | FormalParm -> is_decl decl
  | _ -> []


(* TODO *)
and verify_sc = function
  | _ -> []

and verify_tq = function
  | _ -> []

and verify_asmdq asm = []


let verify tu =
  let errors =
    match tu.d with
    | TranslationUnit (decls) ->
        forall (verify_decl TopLevel) decls
    | _ ->
        verify_node Other tu
  in
  (List.length errors) = 0
