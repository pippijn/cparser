open Ast
open Mach_int


(****************************************************************************
 *
 * Functions with too much logic or reusability to write in the match
 * statements below.
 *
 ****************************************************************************)

(* {{{ resolve_type *)

let resolve_type expr =
  { expr with
    e_type = Type.resolve expr.e_type;
  }

(* }}} *)
(* {{{ check_assignment *)

let check_assignment op lhs rhs =
  let ltype = lhs.e_type in
  let rtype = rhs.e_type in

  if not (Type.is_modifiable_lvalue lhs) then
    die (Expression_error ("left operand must be modifiable lvalue", None, [lhs]));
  let rhs =
    match op with
    | OP_AddAssign
    | OP_SubtractAssign
      when Type.is_pointer ltype.t
        && Type.is_integral rtype.t ->
          if not (Type.is_complete (Type.pointer_base ltype).t) then
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

  let ltype = lhs.e_type in
  let rtype = rhs.e_type in

  (* Canonicalise pointer arithmetic expressions: handle int+ptr as ptr+int. *)
  let ltype, lhs, rtype, rhs =
    match op with
    | OP_Add when Type.is_integral ltype.t && Type.is_pointer rtype.t ->
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
          Type.is_arithmetic ltype.t && Type.is_arithmetic rtype.t ||
          Type.is_pointer ltype.t && Type.is_integral rtype.t
        in
        if not ok then
          false, "operands must have arithmetic type or ptr/int"
        else
          let ok =
            not (Type.is_pointer ltype.t) || Type.is_complete ltype.t
          in
          ok, "pointed-to type is incomplete"

    | OP_Subtract ->
        (* Allow num-num, ptr-int and ptr-ptr. *)
        let ok =
          Type.is_arithmetic ltype.t && Type.is_arithmetic rtype.t ||
          Type.is_pointer ltype.t && Type.is_integral rtype.t ||
          Type.is_pointer ltype.t && Type.is_pointer rtype.t
        in
        ok, "operands have incompatible types"

    (* Numeric operators. *)
    | OP_Divide
    | OP_Multiply ->
        let ok =
          Type.is_arithmetic ltype.t && Type.is_arithmetic rtype.t
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
          Type.is_integral ltype.t && Type.is_integral rtype.t
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
        if Type.is_pointer ltype.t then
          (* At this point, we don't know the value of pointer arithmetic. *)
          ltype, Constant.NonConst
        else
          let lval = lhs.e_cval in
          let rval = rhs.e_cval in

          (* Expression type is that of the left hand side (after conversions and canonicalisation). *)
          ltype, Const_eval.eval_arithmetic op lval rval

    | OP_Subtract ->
        if Type.is_pointer rtype.t then
          (* At this point, we don't know the value of pointer arithmetic. *)
          Platform.ptrdiff_t, Constant.NonConst
        else
          let lval = lhs.e_cval in
          let rval = rhs.e_cval in
          ltype, Const_eval.eval_arithmetic op lval rval

    | OP_BitwiseAnd
    | OP_BitwiseOr
    | OP_BitwiseXor
    | OP_Divide
    | OP_Modulo
    | OP_Multiply
    | OP_ShiftLeft
    | OP_ShiftRight ->
        let lval = lhs.e_cval in
        let rval = rhs.e_cval in
        ltype, Const_eval.eval_arithmetic op lval rval

    | OP_LogicalAnd
    | OP_LogicalOr ->
        let lval = lhs.e_cval in
        let rval = rhs.e_cval in
        { t = PartialBasicType [BT_Bool];
          t_sloc = Location.dummy;
        }, Const_eval.eval_arithmetic op lval rval

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
  func : Ast.decl option;
}


let rec tcheck_struct env = Visit.({
  map_type = tcheck_type env;
  map_expr = tcheck_expr env;
  map_stmt = tcheck_stmt env;
  map_decl = tcheck_decl env;
})

(* {{{ tcheck_decl *)

and tcheck_decl env decl =
  match decl.d with
  | TypedDecl (scope, sc, ty, untyped, asm, init) ->
      let opens_scope, tag, is_sue_definition =
        match ty.t with
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
        Csymtab.enter_scope scope;

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
      let decl = {
        decl with
        d = TypedDecl (scope, sc, ty, untyped, asm, init)
      } in

      (* Insert tag. *)
      maybe_insert Csymtab.insert_decl decl;

      (* Check the rest of the declaration, but don't re-check the SUE members.
       * The type is complete at this point, so the initialiser can use it. *)
      let untyped = tcheck_decl env untyped in
      let asm = tcheck_decl env asm in
      let init = Option.map (tcheck_expr env) init in

      (* Compose the parts into a new declaration. *)
      let decl = {
        decl with
        d = TypedDecl (scope, sc, ty, untyped, asm, init)
      } in

      (* Insert tag again with updated [decl]. *)
      maybe_insert Csymtab.replace_decl decl;

      begin match untyped.d with
      | EmptyDecl ->
          () (* Pure SUE declaration, no declarator. *)
      | _ ->
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

  | FunctionDefinition (decl, body) ->
      (* First type-check declaration. *)
      let decl = tcheck_decl env decl in
      (* While checking the body, [decl] is the enclosing function. *)
      let env = { func = Some decl } in
      (* Check the body with updated [env]. *)
      let body = tcheck_stmt env body in

      { decl with d = FunctionDefinition (decl, body) }

  | _ -> Visit.map_decl (tcheck_struct env) decl

(* }}} *)
(* {{{ tcheck_stmt *)

and tcheck_stmt env stmt =
  match stmt.s with
  (* Handle CompoundStatement before [map_stmt] so we can enter its scope. *)
  | CompoundStatement (scope, _) ->
      Csymtab.enter_scope scope;
      let stmt = Visit.map_stmt (tcheck_struct env) stmt in
      Csymtab.leave_scope ();
      stmt

  | _ ->
      let stmt = Visit.map_stmt (tcheck_struct env) stmt in
      match stmt.s with
      | ExpressionStatement _ -> stmt

      | ReturnStatement (None) ->
          let retty = Decls.return_type (Option.get env.func) in
          if not (Type.is_void retty.t) then
            die (Statement_error ("non-void function should return a value",
                                  None, [stmt]));
          stmt

      | ReturnStatement (Some expr) ->
          let retty = Decls.return_type (Option.get env.func) in
          if Type.is_void retty.t then
            die (Expression_error ("void function should not return a value", None, [expr]));
          { stmt with
            s = ReturnStatement (Some (Conversions.return expr retty)) }

      | _ -> die (Statement_error ("unhandled statement in type check", None,
                                   [stmt]))

(* }}} *)
(* {{{ tcheck_expr *)

and tcheck_expr env untyped =
  let untyped = Visit.map_expr (tcheck_struct env) untyped in

  match untyped.e with
  | FloatingLiteral (kind, str, suffix) ->
      { untyped with
        e_type = Tcheck.float_type suffix;
        e_cval = Const_eval.parse_float kind str;
      }

  | IntegerLiteral (kind, str, suffix) ->
      { untyped with
        e_type = Tcheck.int_type suffix;
        e_cval = Const_eval.parse_int kind str;
      }

  | StringLiteral (kind, strlist) ->
      let len, chars = Const_eval.parse_string_literal strlist in
      let len =
        match kind with
        | LIT_String -> String.length chars
        | LIT_WString -> len
      in
      { untyped with
        e_type = Tcheck.string_type len kind chars;
        e_cval = Const_eval.make_string kind chars;
      }

  | BinaryExpression (op, lhs, rhs) ->
      let lhs = resolve_type lhs in
      let rhs = resolve_type rhs in

      let lhs, rhs, exprtype, exprval =
        if Operator.is_assignment op then check_assignment op lhs rhs
        else if Operator.is_arithmetic op then check_arithmetic op lhs rhs
        else die (Expression_error ("unhandled binary operator", None, [untyped]))
      in

      { untyped with
        e = BinaryExpression (op, lhs, rhs);
        e_type = exprtype;
        e_cval = exprval;
      }

  | Cast (ty, expr) ->
      { untyped with
        e_type = ty;
        e_cval = expr.e_cval;
      }

  | SizeofExpr (expr) ->
      { untyped with
        e_type = Platform.size_t;
        e_cval = Constant.of_int (Type.sizeof expr.e_type);
      }

  | SizeofType (ty) ->
      { untyped with
        e_type = Platform.size_t;
        e_cval = Constant.of_int (Type.sizeof ty);
      }

  | AlignofExpr (expr) ->
      { untyped with
        e_type = Platform.size_t;
        e_cval = Constant.of_int (Type.alignof expr.e_type);
      }

  | AlignofType (ty) ->
      { untyped with
        e_type = Platform.size_t;
        e_cval = Constant.of_int (Type.alignof ty);
      }

  | ArrayAccess (expr, index) ->
      let expr = Conversions.unary expr in
      { untyped with
        e = ArrayAccess (expr, index);
        e_type = Type.pointer_base expr.e_type;
        e_cval = Constant.NonConst;
      }

  | InitialiserList _ -> untyped

  | Identifier (id) ->
      let sym =
        try
          Csymtab.lookup_decl id Symtab.Ordinary
        with Not_found ->
          die (Expression_error ("identifier '" ^ id ^ "' not declared in this scope",
                                 Some "6.2.1p2", [untyped]))
      in
      let ty, value =
        match sym.d with
        | Enumerator (_, Some { e_type = ty; e_cval = value }) ->
            ty, value
        | _ ->
            Decls.decl_type sym, Constant.NonConst
      in
      { untyped with
        e_type = ty;
        e_cval = value;
      }

  | _ -> die (Expression_error ("unhandled expression in type check", None,
                                [untyped]))

(* }}} *)
(* {{{ tcheck_type *)

and tcheck_type env untyped =
  let ty = Visit.map_type (tcheck_struct env) untyped in
  match ty.t with
  | SUEType (attrs, SUE_Enum, tag, members) ->
      let _, members =
        List.fold_left (fun (value, members) enum ->
          let value, enum, name =
            match enum.d with
            | Enumerator (name, None) ->
                (succ_mach_int value,
                 { enum with
                   d = Enumerator (name, Some (Const_eval.make_int value))
                 },
                 name)
            | Enumerator (name, Some value) ->
                (succ_mach_int (Constant.to_mach_int value.e_cval),
                 enum,
                 name)
            | _ -> die (Declaration_error (
                "invalid declaration in enum", None, [enum]))
          in
          Csymtab.insert_decl name Symtab.Ordinary enum;
          value, enum :: members
        ) (zero_mach_int, []) members
      in
      { ty with
        t = SUEType (attrs, SUE_Enum, tag, List.rev members);
      }

  | _ -> ty

(* }}} *)

let tcheck_unit = tcheck_decl {
  func = None;
}
(*let tcheck_unit a b = b*)
