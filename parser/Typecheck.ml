open Ast
open Mach_int


let value_of = Type.value_of
let type_of = Type.type_of

(****************************************************************************
 *
 * Functions with too much logic or reusability to write in the match
 * statements below.
 *
 ****************************************************************************)

(* {{{ resolve_type *)

let resolve_type = function
  | TypedExpression (ty, value, expr) ->
      TypedExpression (Type.resolve ty, value, expr)
  | expr -> die (Expression_error (",,", None, [expr]))

(* }}} *)
(* {{{ check_assignment *)

let check_assignment op lhs rhs =
  let ltype = type_of lhs in
  let rtype = type_of rhs in

  if not (Type.is_modifiable_lvalue lhs) then
    die (Expression_error ("left operand must be modifiable lvalue", None, [lhs]));
  let rhs =
    match op with
    | OP_AddAssign
    | OP_SubtractAssign
      when Type.is_pointer ltype
        && Type.is_integral rtype ->
          if not (Type.is_complete (Type.pointer_base ltype)) then
            die (Expression_error ("cannot perform pointer arithmetic on incomplete type", None, [lhs]));
          rhs
    | _ ->
        Conversions.assignment rhs ltype
  in
  (* The type of the expression is the type of the LHS.
   * The value is unknown, even if the RHS is known,
   * since the LHS is non-constant. *)
  lhs, rhs, ltype, Constant.NonConst

(* }}} *)
(* {{{ check_arithmetic *)

let check_arithmetic op lhs rhs =
  (* Usual unary conversions for ltype and rtype *)
  let lhs = Conversions.unary lhs in
  let rhs = Conversions.unary rhs in
  (* Usual binary conversions unless op is a shift operator *)
  let lhs, rhs =
    match op with
    | OP_ShiftLeft
    | OP_ShiftRight ->
        lhs, rhs
    | _ -> Conversions.binary lhs rhs
  in

  let ltype = type_of lhs in
  let rtype = type_of rhs in

  (* Canonicalise pointer arithmetic expressions: handle int+ptr as ptr+int. *)
  let ltype, lhs, rtype, rhs =
    match op with
    | OP_Add when Type.is_integral ltype && Type.is_pointer rtype ->
        rtype, rhs, ltype, lhs
    | _ ->
        ltype, lhs, rtype, rhs
  in


  (* Check compatibility in arithmetic. *)
  let ok, msg =
    match op with
    | OP_Add ->
        (* Either both operands should have numeric types or one is pointer
         * and the other is integral. *)
        let ok =
          Type.is_arithmetic ltype && Type.is_arithmetic rtype ||
          Type.is_pointer ltype && Type.is_integral rtype
        in
        if not ok then
          false, "operands must have arithmetic type or ptr/int"
        else
          let ok =
            not (Type.is_pointer ltype) || Type.is_complete ltype
          in
          ok, "pointed-to type is incomplete"

    | OP_Subtract ->
        (* Allow num-num, ptr-int and ptr-ptr. *)
        let ok =
          Type.is_arithmetic ltype && Type.is_arithmetic rtype ||
          Type.is_pointer ltype && Type.is_integral rtype ||
          Type.is_pointer ltype && Type.is_pointer rtype
        in
        ok, "operands have incompatible types"

    (* Numeric operators. *)
    | OP_Divide
    | OP_Multiply ->
        let ok =
          Type.is_arithmetic ltype && Type.is_arithmetic rtype
        in
        ok, "operands must have arithmetic type"

    (* Integral operators. *)
    | OP_Modulo
    | OP_BitwiseAnd
    | OP_BitwiseOr
    | OP_BitwiseXor
    | OP_ShiftLeft
    | OP_ShiftRight ->
        let ok =
          Type.is_integral ltype && Type.is_integral rtype
        in
        ok, "operands must have integral type"

    | OP_LogicalAnd
    | OP_LogicalOr ->
        let ok =
          true (* TODO: check conversion to bool *)
        in
        ok, "operands must be convertible to bool"

    | _ ->
        false, "unhandled operator"
  in
  if not ok then
    die (Expression_error (msg ^ " in binary operator `" ^ (Token.string_of_binop op) ^ "'", None, [lhs; rhs]));


  (* Perform arithmetic. *)
  let exprtype, exprval =
    match op with
    | OP_Add ->
        if Type.is_pointer ltype then
          (* At this point, we don't know the value of pointer arithmetic. *)
          ltype, Constant.NonConst
        else
          let lval = value_of lhs in
          let rval = value_of rhs in

          (* Expression type is that of the left hand side (after conversions and canonicalisation). *)
          ltype, Const_eval.eval_arithmetic op lval rval

    | OP_Subtract ->
        if Type.is_pointer rtype then
          (* At this point, we don't know the value of pointer arithmetic. *)
          Platform.ptrdiff_t, Constant.NonConst
        else
          let lval = value_of lhs in
          let rval = value_of rhs in
          ltype, Const_eval.eval_arithmetic op lval rval

    | OP_BitwiseAnd
    | OP_BitwiseOr
    | OP_BitwiseXor
    | OP_Divide
    | OP_Modulo
    | OP_Multiply
    | OP_ShiftLeft
    | OP_ShiftRight ->
        let lval = value_of lhs in
        let rval = value_of rhs in
        ltype, Const_eval.eval_arithmetic op lval rval

    | OP_LogicalAnd
    | OP_LogicalOr ->
        let lval = value_of lhs in
        let rval = value_of rhs in
        PartialBasicType [BT_Bool], Const_eval.eval_arithmetic op lval rval

    | _ -> die (Unimplemented "check_arithmetic")
  in

  lhs, rhs, exprtype, exprval

(* }}} *)

(****************************************************************************
 *
 * Main type-check visitor.
 *
 ****************************************************************************)


type env = {
  (* Enclosing function. *)
  func : Ast.declaration option;
}


let rec tcheck_struct env = Visit.({
  map_type = tcheck_type env;
  map_expr = tcheck_expr env;
  map_stmt = tcheck_stmt env;
  map_decl = tcheck_decl env;
})

(* {{{ tcheck_decl *)

and tcheck_decl env = function
  | TypedDecl (trs, sc, ty, untyped, asm, init) ->
      let opens_scope, tag, is_sue_definition =
        match ty with
        | SUEType (_, suekind, tag, members) ->
            let opens_scope =
              match suekind with
              (* [enum] doesn't open a new scope. All its members are inserted into
               * the enclosing scope. *)
              | SUE_Enum -> false
              | SUE_Struct | SUE_Union -> true
            in
            (* If the type has members, it is a definition. *)
            let is_sue_definition = members <> [] in

            opens_scope, Some tag, is_sue_definition
        (* Any other type does not open a scope, has no tag and is not a SUE definition. *)
        | _ ->
            false, None, false
      in

      if opens_scope then
        Csymtab.enter_scope (Attributes.scope trs);

      (* First check type. *)
      let ty = tcheck_type env ty in

      (* Leave struct/union scope. *)
      if opens_scope then
        Csymtab.leave_scope ();

      (* Insert a tag into the symbol table, iff the type is a complete SUE definition. *)
      let maybe_insert insert decl =
        if is_sue_definition then insert (Option.get tag) Symtab.Tag decl
      in

      (* Create a temporary declaration so we may have a type-checked SUE body
       * when checking the initialiser. *)
      let decl = TypedDecl (trs, sc, ty, untyped, asm, init) in

      (* Insert tag. *)
      maybe_insert Csymtab.insert_decl decl;

      (* Check the rest of the declaration, but don't re-check the SUE members.
       * The type is complete at this point, so the initialiser can use it. *)
      let untyped = tcheck_decl env untyped in
      let asm = tcheck_decl env asm in
      let init = Option.map (tcheck_expr env) init in

      (* Compose the parts into a new declaration. *)
      let decl = TypedDecl (trs, sc, ty, untyped, asm, init) in

      (* Insert tag again with updated [decl]. *)
      maybe_insert Csymtab.replace_decl decl;

      begin match untyped with
      | EmptyDecl ->
          () (* Pure SUE declaration, no declarator. *)
      | untyped ->
          Csymtab.insert_decl (Decls.decl_name untyped) Symtab.Ordinary decl
      end;

      begin if Option.is_some init then
        if Sclass.is_typedef sc then
        (* Typedefs cannot have initialisers. *)
          die (Declaration_error ("typedefs cannot have initialisers", None, [decl]))
        else
          (* If there is no enclosing function, we are at toplevel scope. *)
          let toplevel = Option.is_none env.func in
          let declty = Type.resolve (Decls.decl_type decl) in
          Initialiser.check_decl_init decl declty toplevel
      end;

      decl

  | FunctionDefinition (trs, decl, body) ->
      (* First type-check declaration. *)
      let decl = tcheck_decl env decl in
      (* While checking the body, [decl] is the enclosing function. *)
      let env = { func = Some decl } in
      (* Check the body with updated [env]. *)
      let body = tcheck_stmt env body in

      FunctionDefinition (trs, decl, body)

  | n -> Visit.map_decl (tcheck_struct env) n

(* }}} *)
(* {{{ tcheck_stmt *)

and tcheck_stmt env = function
  (* Handle CompoundStatement before [map_stmt] so we can enter its scope. *)
  | CompoundStatement (trs, _) as n ->
      Csymtab.enter_scope (Attributes.scope trs);
      let n = Visit.map_stmt (tcheck_struct env) n in
      Csymtab.leave_scope ();
      n

  | n ->
      match Visit.map_stmt (tcheck_struct env) n with
      | ExpressionStatement _ as n -> n

      | ReturnStatement (trs, None) as n ->
          let retty = Decls.return_type (Option.get env.func) in
          if not (Type.is_void retty) then
            die (Statement_error ("non-void function should return a value", None, [n]));
          n

      | ReturnStatement (trs, Some expr) ->
          let retty = Decls.return_type (Option.get env.func) in
          if Type.is_void retty then
            die (Expression_error ("void function should not return a value", None, [expr]));
          ReturnStatement (trs, Some (Conversions.return expr retty))

      | n -> die (Statement_error ("unhandled statement in type check", None, [n]))

(* }}} *)
(* {{{ tcheck_expr *)

and tcheck_expr env untyped =
  match Visit.map_expr (tcheck_struct env) untyped with
  | FloatingLiteral (_, kind, str, suffix) as lit ->
      TypedExpression (Tcheck.float_type suffix, Const_eval.parse_float kind str, lit)

  | IntegerLiteral (_, kind, str, suffix) as lit ->
      TypedExpression (Tcheck.int_type suffix, Const_eval.parse_int kind str, lit)

  | StringLiteral (_, kind, strlist) as lit ->
      let len, chars = Const_eval.parse_string_literal strlist in
      let len =
        match kind with
        | LIT_String -> String.length chars
        | LIT_WString -> len
      in
      TypedExpression (Tcheck.string_type len kind chars, Const_eval.make_string kind chars, lit)

  | BinaryExpression (trs, op, lhs, rhs) as expr ->
      let lhs = resolve_type lhs in
      let rhs = resolve_type rhs in

      let lhs, rhs, exprtype, exprval =
        if Operator.is_assignment op then check_assignment op lhs rhs
        else if Operator.is_arithmetic op then check_arithmetic op lhs rhs
        else die (Expression_error ("unhandled binary operator", None, [expr]))
      in

      TypedExpression (exprtype, exprval, BinaryExpression (trs, op, lhs, rhs))

  | Cast (_, ty, expr) as cast ->
      TypedExpression (ty, value_of expr, cast)

  | SizeofExpr (_, expr) as sizeof ->
      TypedExpression (Platform.size_t, Constant.of_int (Type.sizeof (type_of expr)), sizeof)

  | SizeofType (_, ty) as sizeof ->
      TypedExpression (Platform.size_t, Constant.of_int (Type.sizeof ty), sizeof)

  | AlignofExpr (_, expr) as alignof ->
      TypedExpression (Platform.size_t, Constant.of_int (Type.alignof (type_of expr)), alignof)

  | AlignofType (_, ty) as alignof ->
      TypedExpression (Platform.size_t, Constant.of_int (Type.alignof ty), alignof)

  | ArrayAccess (trs, expr, index) ->
      let expr = Conversions.unary expr in
      TypedExpression (Type.pointer_base (type_of expr), Constant.NonConst, ArrayAccess (trs, expr, index))

  | InitialiserList _ as n -> n

  | Identifier (_, id) as n ->
      let sym =
        try
          Csymtab.lookup_decl id Symtab.Ordinary
        with Not_found ->
          die (Expression_error ("identifier '" ^ id ^ "' not declared in this scope", Some "6.2.1p2", [n]))
      in
      let ty, value =
        match sym with
        | Enumerator (_, _, Some (TypedExpression (ty, value, _))) -> ty, value
        | decl -> Decls.decl_type decl, Constant.NonConst
      in
      TypedExpression (ty, value, n)

  | n -> die (Expression_error ("unhandled expression in type check", None, [n]))

(* }}} *)
(* {{{ tcheck_type *)

and tcheck_type env untyped =
  match Visit.map_type (tcheck_struct env) untyped with
  | SUEType (attrs, SUE_Enum, tag, members) ->
      let _, members =
        List.fold_left (fun (value, members) enum ->
          let value, enum, name =
            match enum with
            | Enumerator (trs, name, None) ->
                succ_mach_int value, Enumerator (trs, name, Some (Const_eval.make_int value)), name
            | Enumerator (_, name, Some value) as enum ->
                succ_mach_int (Constant.to_mach_int (value_of value)), enum, name
            | decl -> die (Declaration_error ("invalid declaration in enum", None, [decl]))
          in
          Csymtab.insert_decl name Symtab.Ordinary enum;
          value, enum :: members
        ) (zero_mach_int, []) members
      in
      SUEType (attrs, SUE_Enum, tag, List.rev members)

  | n -> n

(* }}} *)

let tcheck_unit = tcheck_decl {
  func = None;
}
(*let tcheck_unit a b = b*)
