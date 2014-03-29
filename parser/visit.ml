open Ast


(* {{{ *)

let iter = List.iter

type iter_struct = {
  iter_type : Ast.ctyp -> unit;
  iter_expr : Ast.expr -> unit;
  iter_stmt : Ast.stmt -> unit;
  iter_decl : Ast.decl -> unit;
}


let opt = Option.may


let iter_stmt { iter_type; iter_expr; iter_stmt; iter_decl } node =
  match node.s with
  | EmptyStmt -> ()

  (* Statements *)
  | CompoundStatement (_, stmts) -> iter iter_stmt stmts
  | ExpressionStatement (expr) -> opt iter_expr expr
  | DeclarationStatement (decl) -> iter_decl decl

  (* Labelled statements *)
  | LabelledStatement (_, stmt) -> iter_stmt stmt
  | LocalLabel _ -> ()
  | CaseStatement (expr) -> iter_expr expr
  | DefaultStatement -> ()

  (* Selection statements *)
  | IfStatement (cond, then_stmt, else_stmt) -> iter_expr cond; iter_stmt then_stmt; iter_stmt else_stmt
  | SwitchStatement (expr, cases) -> iter_expr expr; iter_stmt cases

  (* Iteration statements *)
  | WhileStatement (cond, body) -> iter_expr cond; iter_stmt body
  | DoWhileStatement (body, cond) -> iter_stmt body; iter_expr cond
  | ForStatement (init, cond, next, body) -> opt iter_expr init; opt iter_expr cond; opt iter_expr next; iter_stmt body

  (* Jump statements *)
  | GotoStatement _ -> ()
  | ContinueStatement -> ()
  | BreakStatement -> ()
  | ReturnStatement (expr) -> opt iter_expr expr

  (* GCC asm statement *)
  | AsmStatement (volatile, code, in_args, out_args, clobber, labels) ->
      let iter_arg (AsmArgument (_, constr, expr)) = iter_expr expr in
      iter iter_arg in_args;
      iter iter_arg out_args


let iter_expr { iter_type; iter_expr; iter_stmt; iter_decl } node =
  match node.e with
  (* Wildcards *)
  | WildcardExpr _ -> ()

  (* Expression with type information *)
  | TypedExpression (ty, value, expr) -> iter_type ty; iter_expr expr

  (* Expression *)
  | TernaryExpression (op, cond, then_expr, else_expr) -> iter_expr cond; opt iter_expr then_expr; iter_expr else_expr
  | BinaryExpression (op, lhs, rhs) -> iter_expr lhs; iter_expr rhs
  | UnaryExpression (op, expr) -> iter_expr expr
  | ArrayAccess (expr, index) -> iter_expr expr; iter_expr index
  | MemberAccess (expr, member) -> iter_expr expr
  | PointerAccess (expr, member) -> iter_expr expr
  | SizeofExpr (expr) -> iter_expr expr
  | SizeofType (ty) -> iter_type ty
  | AlignofExpr (expr) -> iter_expr expr
  | AlignofType (ty) -> iter_type ty
  | Offsetof (ty, member) -> iter_type ty; iter_expr member
  | TypesCompatibleP (ty1, ty2) -> iter_type ty1; iter_type ty2
  | VaArg (ap, ty) -> iter_expr ap; iter_type ty
  | FunctionCall (callee, args) -> iter_expr callee; iter iter_expr args
  | CompoundLiteral (ty, init) -> iter_type ty; iter_expr init

  (* Primary expression *)
  | Identifier _
  | IntegerLiteral _
  | FloatingLiteral _
  | CharLiteral _
  | StringLiteral _ -> ()
  | BraceExpression (stmt) -> iter_stmt stmt

  (* Cast expression *)
  | Cast (ty, expr) -> iter_type ty; iter_expr expr

  (* Initialisers *)
  | InitialiserList (inits) -> iter iter_expr inits
  | ArrayLabelledInitialiser (index, init) -> iter_expr index; iter_expr init
  | DesignatedInitialiser (designator, init) -> iter_expr init


let iter_type { iter_type; iter_expr; iter_stmt; iter_decl } node =
  match node with
  | EmptyType -> ()

  (* Wildcards *)
  | WildcardType _ -> ()

  (* Types *)
  | PartialBasicType _ -> ()
  | BasicType _ -> ()
  | QualifiedType (tquals, unqual) -> iter_type unqual
  | PointerType (base) -> iter_type base
  | SUEType (_, kind, tag, members) -> iter iter_decl members
  | TypedefType _ -> ()
  | ArrayType (arity, base) -> opt iter_expr arity; iter_type base
  | FunctionType (rettype, params) -> iter_type rettype; iter iter_decl params
  | TypeofExpr (expr) -> iter_expr expr
  | TypeofType (ty) -> iter_type ty


let iter_decl { iter_type; iter_expr; iter_stmt; iter_decl } node =
  match node with
  | EmptyDecl -> ()
  | TranslationUnit (decls) -> iter iter_decl decls

  (* Wildcards *)
  | WildcardDecl _ -> ()

  (* #include etc. *)
  | PreprocessorDirective _ -> ()

  (* Syntax errors *)
  | SyntaxError (_, msg, node) -> iter_decl node

  (* Toplevel __asm__ *)
  | ToplevelAsm _ -> ()

  (* Generic nodes *)
  | AsmSpecifier _ -> ()
  | FunctionDefinition (_, decl, body) -> iter_decl decl; iter_stmt body
  | IdentifierDeclarator _ -> ()
  | StructDeclarator (_, decl, bitfield) -> iter_decl decl; opt iter_expr bitfield
  | TypedDecl (_, sclasses, ty, untyped, asm, init) -> iter_type ty; iter_decl untyped; iter_decl asm; opt iter_expr init
  | DeclaringList (_, decls) -> iter iter_decl decls

  (* Struct/union/enum types *)
  | Enumerator (_, id, value) -> opt iter_expr value


(* }}} *)
(* {{{ *)

let map = List.map

type map_struct = {
  map_type : Ast.ctyp -> Ast.ctyp;
  map_expr : Ast.expr -> Ast.expr;
  map_stmt : Ast.stmt -> Ast.stmt;
  map_decl : Ast.decl -> Ast.decl;
}


let opt = Option.map


let map_stmt { map_type; map_expr; map_stmt; map_decl } node =
  { node with
    s =
      match node.s with
      | EmptyStmt -> EmptyStmt

                       (* Statements *)
      | CompoundStatement (scope, stmts) -> CompoundStatement (scope, map map_stmt stmts)
      | ExpressionStatement (expr) -> ExpressionStatement (opt map_expr expr)
      | DeclarationStatement (decl) -> DeclarationStatement (map_decl decl)

                                         (* Labelled statements *)
      | LabelledStatement (label, stmt) -> LabelledStatement (label, map_stmt stmt)
      | LocalLabel _ -> node.s
      | CaseStatement (expr) -> CaseStatement (map_expr expr)
      | DefaultStatement -> node.s

                                (* Selection statements *)
      | IfStatement (cond, then_stmt, else_stmt) -> IfStatement (map_expr cond, map_stmt then_stmt, map_stmt else_stmt)
      | SwitchStatement (expr, cases) -> SwitchStatement (map_expr expr, map_stmt cases)

                                                (* Iteration statements *)
      | WhileStatement (cond, body) -> WhileStatement (map_expr cond, map_stmt body)
      | DoWhileStatement (body, cond) -> DoWhileStatement (map_stmt body, map_expr cond)
      | ForStatement (init, cond, next, body) -> ForStatement (opt map_expr init, opt map_expr cond, opt map_expr next, map_stmt body)

                                                        (* Jump statements *)
      | GotoStatement _ -> node.s
      | ContinueStatement -> node.s
      | BreakStatement -> node.s
      | ReturnStatement (expr) -> ReturnStatement (opt map_expr expr)

                                         (* GCC asm statement *)
      | AsmStatement (volatile, code, in_args, out_args, clobber, labels) ->
          let map_arg (AsmArgument (trs, constr, expr)) = AsmArgument (trs, constr, map_expr expr) in
          AsmStatement (volatile, code, map map_arg in_args, map map_arg out_args, clobber, labels)
  }


let map_expr { map_type; map_expr; map_stmt; map_decl } node =
  { node with
    e =
      match node.e with
      | WildcardExpr _ as node -> node

      | TypedExpression (ty, value, expr) -> TypedExpression (map_type ty, value, map_expr expr)

      | TernaryExpression (op, cond, then_expr, else_expr) -> TernaryExpression (op, map_expr cond, opt map_expr then_expr, map_expr else_expr)
      | BinaryExpression (op, lhs, rhs) -> BinaryExpression (op, map_expr lhs, map_expr rhs)
      | UnaryExpression (op, expr) -> UnaryExpression (op, map_expr expr)
      | ArrayAccess (expr, index) -> ArrayAccess (map_expr expr, map_expr index)
      | MemberAccess (expr, member) -> MemberAccess (map_expr expr, member)
      | PointerAccess (expr, member) -> PointerAccess (map_expr expr, member)
      | SizeofExpr (expr) -> SizeofExpr (map_expr expr)
      | SizeofType (ty) -> SizeofType (map_type ty)
      | AlignofExpr (expr) -> AlignofExpr (map_expr expr)
      | AlignofType (ty) -> AlignofType (map_type ty)
      | Offsetof (ty, member) -> Offsetof (map_type ty, map_expr member)
      | TypesCompatibleP (ty1, ty2) -> TypesCompatibleP (map_type ty1, map_type ty2)
      | VaArg (ap, ty) -> VaArg (map_expr ap, map_type ty)
      | FunctionCall (callee, args) -> FunctionCall (map_expr callee, map map_expr args)
      | CompoundLiteral (ty, init) -> CompoundLiteral (map_type ty, map_expr init)

      | Identifier _
      | IntegerLiteral _
      | FloatingLiteral _
      | CharLiteral _
      | StringLiteral _ as node -> node
      | BraceExpression (stmt) -> BraceExpression (map_stmt stmt)

      | Cast (ty, expr) -> Cast (map_type ty, map_expr expr)

      | InitialiserList (inits) -> InitialiserList (map map_expr inits)
      | ArrayLabelledInitialiser (index, init) -> ArrayLabelledInitialiser (map_expr index, map_expr init)
      | DesignatedInitialiser (designator, init) -> DesignatedInitialiser (designator, map_expr init)
  }


let map_type { map_type; map_expr; map_stmt; map_decl } = function
  | EmptyType -> EmptyType

  (* Wildcards *)
  | WildcardType _ as node -> node

  (* Types *)
  | PartialBasicType _ as node -> node
  | BasicType _ as node -> node
  | QualifiedType (tquals, unqual) -> QualifiedType (tquals, map_type unqual)
  | PointerType (base) -> PointerType (map_type base)
  | SUEType (attr, kind, tag, members) -> SUEType (attr, kind, tag, map map_decl members)
  | TypedefType _ as node -> node
  | ArrayType (arity, base) -> ArrayType (opt map_expr arity, map_type base)
  | FunctionType (rettype, params) -> FunctionType (map_type rettype, map map_decl params)
  | TypeofExpr (expr) -> TypeofExpr (map_expr expr)
  | TypeofType (ty) -> TypeofType (map_type ty)


let map_decl { map_type; map_expr; map_stmt; map_decl } node =
  match node with
  | EmptyDecl -> node
  | TranslationUnit (decls) -> TranslationUnit (map map_decl decls)

  (* Wildcards *)
  | WildcardDecl _ -> node

  (* #include etc. *)
  | PreprocessorDirective _ -> node

  (* Syntax errors *)
  | SyntaxError (trs, msg, node) -> SyntaxError (trs, msg, map_decl node)

  (* Statements *)
  | ToplevelAsm _ -> node

  (* Generic nodes *)
  | AsmSpecifier _ -> node
  | FunctionDefinition (trs, decl, body) -> FunctionDefinition (trs, map_decl decl, map_stmt body)
  | IdentifierDeclarator _ -> node
  | StructDeclarator (trs, decl, bitfield) -> StructDeclarator (trs, map_decl decl, opt map_expr bitfield)
  | TypedDecl (trs, sclasses, ty, untyped, asm, init) -> TypedDecl (trs, sclasses, map_type ty, map_decl untyped, map_decl asm, opt map_expr init)
  | DeclaringList (trs, decls) -> DeclaringList (trs, map map_decl decls)

  (* Struct/union/enum types *)
  | Enumerator (trs, id, value) -> Enumerator (trs, id, opt map_expr value)


(* }}} *)
(* {{{ *)

let fold_left = List.fold_left

type 'a fold_struct = {
  fold_type : 'a -> Ast.ctyp -> 'a;
  fold_expr : 'a -> Ast.expr -> 'a;
  fold_stmt : 'a -> Ast.stmt -> 'a;
  fold_decl : 'a -> Ast.decl -> 'a;
}


let opt = Option.fold

let (|>) x (f, a) = f x a


let fold_stmt { fold_type; fold_expr; fold_stmt; fold_decl } data node =
  match node.s with
  | EmptyStmt -> data

  (* Statements *)
  | CompoundStatement (_, stmts) -> fold_left fold_stmt data stmts
  | ExpressionStatement (expr) -> opt fold_expr data expr
  | DeclarationStatement (decl) -> fold_decl data decl

  (* Labelled statements *)
  | LabelledStatement (label, stmt) -> fold_stmt data stmt
  | LocalLabel _ -> data
  | CaseStatement (expr) -> fold_expr data expr
  | DefaultStatement -> data

  (* Selection statements *)
  | IfStatement (cond, then_stmt, else_stmt) ->
      data |> (fold_expr, cond) |> (fold_stmt, then_stmt) |> (fold_stmt, else_stmt)
  | SwitchStatement (expr, cases) ->
      data |> (fold_expr, expr) |> (fold_stmt, cases)

  (* Iteration statements *)
  | WhileStatement (cond, body) ->
      data |> (fold_expr, cond) |> (fold_stmt, body)
  | DoWhileStatement (body, cond) ->
      data |> (fold_stmt, body) |> (fold_expr, cond)
  | ForStatement (init, cond, next, body) ->
      data |> (opt fold_expr, init) |> (opt fold_expr, cond) |> (opt fold_expr, next) |> (fold_stmt, body)

  (* Jump statements *)
  | GotoStatement _ -> data
  | ContinueStatement -> data
  | BreakStatement -> data
  | ReturnStatement (expr) -> opt fold_expr data expr

  (* GCC asm statement *)
  | AsmStatement (volatile, code, in_args, out_args, clobber, labels) ->
      let fold_arg data (AsmArgument (_, constr, expr)) = fold_expr data expr in
      let fold = fold_left fold_arg in
      data |> (fold, in_args) |> (fold, out_args)


let fold_expr { fold_type; fold_expr; fold_stmt; fold_decl } data node =
  match node with
  (* Wildcards *)
  | WildcardExpr _ -> data

  (* Expression with type information *)
  | TypedExpression (ty, value, expr) ->
      data |> (fold_type, ty) |> (fold_expr, expr)

  (* Expression *)
  | TernaryExpression (op, cond, then_expr, else_expr) ->
      data |> (fold_expr, cond) |> (opt fold_expr, then_expr) |> (fold_expr, else_expr)
  | BinaryExpression (op, lhs, rhs) ->
      data |> (fold_expr, lhs) |> (fold_expr, rhs)
  | UnaryExpression (op, expr) -> fold_expr data expr
  | ArrayAccess (expr, index) ->
      data |> (fold_expr, expr) |> (fold_expr, index)
  | MemberAccess (expr, member) -> fold_expr data expr
  | PointerAccess (expr, member) -> fold_expr data expr
  | SizeofExpr (expr) -> fold_expr data expr
  | SizeofType (ty) -> fold_type data ty
  | AlignofExpr (expr) -> fold_expr data expr
  | AlignofType (ty) -> fold_type data ty
  | Offsetof (ty, member) ->
      data |> (fold_type, ty) |> (fold_expr, member)
  | TypesCompatibleP (ty1, ty2) ->
      data |> (fold_type, ty1) |> (fold_type, ty2)
  | VaArg (ap, ty) ->
      data |> (fold_expr, ap) |> (fold_type, ty)
  | FunctionCall (callee, args) ->
      data |> (fold_expr, callee) |> (fold_left fold_expr, args)
  | CompoundLiteral (ty, init) ->
      data |> (fold_type, ty) |> (fold_expr, init)

  (* Primary expression *)
  | Identifier _
  | IntegerLiteral _
  | FloatingLiteral _
  | CharLiteral _
  | StringLiteral _ -> data
  | BraceExpression (stmt) -> fold_stmt data stmt

  (* Cast expression *)
  | Cast (ty, expr) -> fold_expr (fold_type data ty) expr

  (* Initialisers *)
  | InitialiserList (inits) -> fold_left fold_expr data inits
  | ArrayLabelledInitialiser (index, init) ->
      data |> (fold_expr, index) |> (fold_expr, init)
  | DesignatedInitialiser (designator, init) ->
      data |> (fold_expr, init)


let fold_type { fold_type; fold_expr; fold_stmt; fold_decl } data node =
  match node with
  | EmptyType -> data

  (* Wildcards *)
  | WildcardType _ -> data

  (* Types *)
  | PartialBasicType _ -> data
  | BasicType _ -> data
  | QualifiedType (tquals, unqual) -> fold_type data unqual
  | PointerType (base) -> fold_type data base
  | SUEType (_, kind, tag, members) -> fold_left fold_decl data members
  | TypedefType _ -> data
  | ArrayType (arity, base) ->
      data |> (opt fold_expr, arity) |> (fold_type, base)
  | FunctionType (rettype, params) ->
      data |> (fold_type, rettype) |> (fold_left fold_decl, params)
  | TypeofExpr (expr) -> fold_expr data expr
  | TypeofType (ty) -> fold_type data ty


let fold_decl { fold_type; fold_expr; fold_stmt; fold_decl } data node =
  match node with
  | EmptyDecl -> data
  | TranslationUnit (decls) -> fold_left fold_decl data decls

  (* Wildcards *)
  | WildcardDecl _ -> data

  (* #include etc. *)
  | PreprocessorDirective _ -> data

  (* Syntax errors *)
  | SyntaxError (_, msg, node) -> fold_decl data node

  (* Toplevel __asm__ *)
  | ToplevelAsm _ -> data

  (* Generic nodes *)
  | AsmSpecifier _ -> data
  | FunctionDefinition (_, decl, body) ->
      data |> (fold_decl, decl) |> (fold_stmt, body)
  | IdentifierDeclarator _ -> data
  | StructDeclarator (_, decl, bitfield) ->
      data |> (fold_decl, decl) |> (opt fold_expr, bitfield)
  | TypedDecl (_, sclasses, ty, untyped, asm, init) ->
      data |> (fold_type, ty) |> (fold_decl, untyped) |> (fold_decl, asm) |> (opt fold_expr, init)
  | DeclaringList (_, decls) -> fold_left fold_decl data decls

  (* Struct/union/enum types *)
  | Enumerator (_, id, value) -> opt fold_expr data value


(* }}} *)
