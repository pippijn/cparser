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


let rec verify_node c = function
  | DeclaringList (_, decls) -> forall (verify_node c) decls
  | PreprocessorDirective (_, dir) -> []

  (*** STATEMENTS ***)
  | ToplevelAsm _ -> []

  (*** STRUCT/UNION/ENUM ***)

  | Enumerator (_, name, expr) ->
      opt verify_expr expr


  (*** DECLS ***)

  (* declarations *)
  | TypedDecl (_, sclasses, declty, decl, EmptyDecl, None) when Sclass.is_empty sclasses ->
      verify_type declty @
      opt_verify_declr c decl
  | TypedDecl (_, sclasses, declty, decl, asm, init) as node ->
      (if c = StructField then bad_decl "bad declaration form in struct member" node else []) @
      verify_sc sclasses @
      verify_type declty @
      opt_verify_declr c decl @
      opt_verify_asmdq asm @
      opt verify_expr init

  | FunctionDefinition (_, ty, body) ->
      (* type of function definitions is a function type *)
      let rec verify ty =
        match ty with
        | TypedDecl (_, _, (FunctionType _ as ty), _, _, _) ->
            verify_type ty
        | _ -> bad_decl "function definition does not have function type" ty
      in
      verify ty @
      verify_stmt body

  | n -> die (Parse_error ("unexpected node", n))


and verify_declr c = function
  | DeclaringList (_, decls) ->
      forall (verify_declr c) decls
  | StructDeclarator (_, decl, bitfield) as node ->
      (if c <> StructField then bad_decl "found struct declarator outside struct" node else []) @
      verify_decl c decl @
      opt verify_expr bitfield
  | WildcardDecl _
  | IdentifierDeclarator _ -> []
  | n -> die (Parse_error ("unexpected declarator", n))


and opt_verify_asmdq = function
  | EmptyDecl -> []
  | n -> verify_asmdq n

and opt_verify_declr c = function
  | EmptyDecl -> []
  | n -> verify_declr c n

and opt_verify_stmt = function
  | EmptyStmt -> []
  | n -> verify_stmt n


and verify_stmt = function
  (*** STATEMENTS ***)
  | CompoundStatement (_, stmts) ->
      forall verify_stmt stmts
  | ExpressionStatement (_, expr) ->
      opt verify_expr expr
  | DeclarationStatement (decl) ->
      verify_decl Other decl
  | IfStatement (_, cond, then_stmt, else_stmt) ->
      verify_expr cond @
      verify_stmt then_stmt @
      opt_verify_stmt else_stmt
  | SwitchStatement (_, cond, body)
  | WhileStatement (_, cond, body)
  | DoWhileStatement (_, body, cond) ->
      verify_stmt body @
      verify_expr cond
  | ForStatement (_, init, cond, next, body) ->
      forall (opt verify_expr) [init; cond; next] @
      verify_stmt body
  | CaseStatement (_, expr) ->
      verify_expr expr
  | DefaultStatement _
  | BreakStatement _
  | ContinueStatement _ ->
      []
  | ReturnStatement (_, expr) ->
      opt verify_expr expr
  | LabelledStatement (_, "", _) as node ->
      bad_stmt "empty label" node
  | GotoStatement (_, label) ->
      verify_expr label
  | LabelledStatement (_, label, stmt) ->
      opt_verify_stmt stmt
  | LocalLabel (_, labels) as node ->
      forall (function "" -> bad_stmt "empty label" node | _ -> []) labels
  | AsmStatement (_, volatile, code, in_regs, out_regs, clobber, labels) ->
      let verify (AsmArgument (_, constr, expr)) = verify_expr expr in
      forall verify in_regs @
      forall verify out_regs

  | n -> bad_stmt "verify_stmt" n



and verify_expr = function
  | TypedExpression (ty, value, expr) -> verify_type ty @ verify_expr expr

  | WildcardExpr (_, "") as node -> bad_expr "empty wildcard" node
  | WildcardExpr (_, _) -> []

  | Identifier (_, "") as node -> bad_expr "empty identifier" node
  | Identifier (_, _) -> []

  | IntegerLiteral (_, _, "", _)
  | FloatingLiteral (_, _, "", _) as node -> bad_expr "empty literal" node
  | IntegerLiteral (_, _, _, Some "") as node -> bad_expr "empty suffix on literal" node
  | FloatingLiteral (_, _, _, Some "") as node -> bad_expr "empty suffix on literal" node
  | IntegerLiteral (_, _, _, _) -> []
  | FloatingLiteral (_, _, _, _) -> []

  | CharLiteral (_, kind, "") as node -> bad_expr "empty char literal" node
  | CharLiteral (_, kind, _) -> []

  | StringLiteral (_, kind, strs) as node ->
      forall (function "" -> bad_expr "empty string literal" node | _ -> []) strs

  | InitialiserList (_, inits) ->
      forall verify_expr inits
  | MemberDesignator (members) as node ->
      forall (function "" -> bad_expr "empty designator" node | _ -> []) members

  | TernaryExpression (_, _, cond, then_expr, else_expr) ->
      verify_expr cond @
      opt verify_expr then_expr @
      verify_expr else_expr
  | ArrayLabelledInitialiser (_, left, right)
  | DesignatedInitialiser (_, left, right)
  | BinaryExpression (_, _, left, right) ->
      forall verify_expr [left; right]
  | UnaryExpression (_, _, expr) ->
      verify_expr expr

  | CompoundLiteral (_, ty, init) ->
      verify_type ty @
      verify_expr init

  | BraceExpression (_, stmt) ->
      verify_stmt stmt

  | ArrayAccess (_, expr, index) ->
      verify_expr expr @
      verify_expr index

  | PointerAccess (_, expr, member)
  | MemberAccess (_, expr, member) ->
      verify_expr expr

  | AlignofType (_, ty)
  | SizeofType (_, ty) ->
      verify_type ty

  | AlignofExpr (_, expr)
  | SizeofExpr (_, expr) ->
      verify_expr expr

  | Cast (_, ty, expr) ->
      verify_type ty @
      verify_expr expr

  | FunctionCall (_, callee, args) ->
      verify_expr callee @
      forall verify_expr args

  | Offsetof (_, ty, member) ->
      verify_type ty @
      verify_expr member

  | VaArg (_, expr, ty) ->
      verify_expr expr @
      verify_type ty

  | TypesCompatibleP (_, ty1, ty2) ->
      verify_type ty1 @
      verify_type ty2


and verify_type = function
  | EmptyType -> bad_type "empty type" EmptyType

  | WildcardType _ -> []

  (*** TYPES ***)

  | PartialBasicType [] as node -> bad_type "empty basic type" node
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

  | TypedefType "" as node -> bad_type "empty typedef type name" node
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
  | SUEType (_, _, _, [EmptyDecl]) -> []

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
    match tu with
    | TranslationUnit (decls) ->
        forall (verify_decl TopLevel) decls
    | n ->
        verify_node Other n
  in
  (List.length errors) = 0
