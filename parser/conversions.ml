open Ast


(* ignore top-level type qualifiers and test the type *)
let type_equal_modulo_cv = Type.equal_qualified false true


let type_assignable_modulo_cv ltype rtype =
  (*
   * K&R2 A7.17: "... or both operands are pointers to functions
   * or objects whose types are the same except for the possible
   * absence of const or volatile in the right operand."  So
   * qualifiers on left must include all qualifiers on right.
   *)
  if
    match ltype, rtype with
    | QualifiedType (tqs1, _), QualifiedType (tqs2, _) ->
        let test f = not (f tqs1) && f tqs2 in

        test Tqual.is_const ||
        test Tqual.is_volatile ||
        test Tqual.is_restrict

    | _ -> false
  then
    false

  else
    (*
     * type qualifiers okay at top-level; remove them and test
     * entire type
     *)
    Type.equal_qualified false true ltype rtype



let unary expr =
  { expr with
    e =
      match expr.e with
      | TypedExpression (ty, value, expr) ->
          let ty =
            match ty with
            (* Convert char and short to int, keeping their signedness. *)
            | BasicType (SChar | SShort) -> BasicType SInt
            | BasicType (UChar | UShort) -> BasicType UInt
            | BasicType Char ->
                if Platform.char_is_signed then
                  BasicType SInt
                else
                  BasicType UInt
            (* Convert arrays to pointers. *)
            | ArrayType (_, base) -> PointerType (base)
            (* Convert functions to pointers to functions. *)
            | FunctionType _ as ty -> PointerType (ty)
            | ty -> ty
          in

          TypedExpression (ty, value, expr)
      | _ -> die (Expression_error ("usual unary conversions", None, [expr]))
  }


let binary a b =
  match a.e, b.e with
  | TypedExpression (lty, lvalue, lexpr), TypedExpression (rty, rvalue, rexpr) ->
      let rank bt =
        match bt with
        | SInt		-> 1
        | UInt		-> 2
        | SLong		-> 3
        | ULong		-> 4
        | SLongLong	-> 5
        | ULongLong	-> 6
        | Float		-> 7
        | Double	-> 8
        | LongDouble	-> 9
        | bt		-> die (Type_error ("invalid basic type", None, [BasicType bt]))
      in

      let convert to_ty expr =
        match expr.e with
        | TypedExpression (from_ty, evalue, expr) ->
            (* Convert value to floating point, if the target type is a
             * floating point type. *)
            let value =
              if Type.is_floating to_ty && not (Type.is_floating from_ty) then
                let open Constant in
                match evalue with
                | IntValue i ->
                    FloatValue (Mach_int.float_of_mach_int i)
                | _ -> failwith "conversion"
              else
                evalue
            in

            TypedExpression (to_ty, value, expr)
        | _ ->
            die (Expression_error ("untyped expression in implicit conversion", None, [expr]))
      in

      begin match lty, rty with
      (* If the types are already equal, no conversion is required. *)
      | lty, rty when Type.equal lty rty -> a, b

      (* For basic types, convert the smaller type to the bigger type. *)
      | BasicType lbt, BasicType rbt ->
          begin match rank lbt < rank rbt with
          | true  -> (* Convert left expression to right type. *)
              { a with e = convert rty a }, b
          | false -> (* Convert right expression to left type. *)
              a, { b with e = convert lty b }
          end

      (* Complex types undergo no conversions. *)
      | _ ->
          a, b
      end

  | _ -> die (Expression_error ("usual binary conversions", None, [a; b]))


(**
 * [coerce] tests whether assigning rhs to an object of ltype
 * is valid.
 *
 * Pointer/pointer and int/pointer coercions (except for constant 0)
 * are disallowed unless the caller explicitly specifies that they are
 * allowable:
 *
 * [ptr2intp == true] implies that pointers can be coerced to integral types
 * and back.
 *
 * [ptr2ptrp == true]  implies that pointers can be coerced regardless of
 * their base type.
 *
 * [ltype] is restricted to types which make sense in a cast.  In particular,
 * [ltype] may not be an array or function type (though it may be a pointer
 * to an array or function).
 *)
let rec coerce ptr2intp ptr2ptrp rhs ltype =
  let type_error_if msg f l r =
    if f l r then
      die (Type_error (msg, None, [l; r]))
    else
      false
  in

  let expr_error_if msg f l r =
    if f l r then
      die (Expression_error (msg, None, [r]))
    else
      false
  in

  let rtype = Type.type_of rhs in


  if Type.is_arithmetic rtype && Type.is_arithmetic ltype then
    rhs


  else if Type.is_pointer rtype && Type.is_pointer ltype then
    if ptr2ptrp then
      rhs
    else
      let lbasetype = Type.pointer_base ltype in
      let rbasetype = Type.pointer_base rtype in

      let assignable =
        (* Valid conversions. *)
        Type.equal lbasetype rbasetype ||
        Type.is_void lbasetype && Type.is_void rbasetype ||
        type_assignable_modulo_cv lbasetype rbasetype ||

        (* Invalid conversions. *)
        type_error_if "discarding type qualifiers in pointer assignment"
          type_equal_modulo_cv lbasetype rbasetype ||
        type_error_if "pointer base types have different sign"
          type_assignable_modulo_cv (Type.sans_sign lbasetype) (Type.sans_sign rbasetype) ||
        type_error_if "assignment of pointers with different base types"
          (fun _ _ -> true) lbasetype rbasetype
      in

      if assignable then
        rhs
      else
        failwith "no way to assignment-coerce types"


  else if Type.is_integral rtype && Type.is_pointer ltype then
    if ptr2intp || Const_eval.is_zero rhs then
      rhs
    else
      die (Expression_error ("assignment of integer value to pointer", None, [rhs]))


  else if Type.is_pointer ltype && Type.is_integral rtype then
    if ptr2intp then
      rhs
    else
      die (Expression_error ("assignment of pointer value to integer", None, [rhs]))


  else if Type.is_aggregate ltype then
    if
      Type.equal rtype ltype ||
      expr_error_if "coercing to union member"
        (fun l r -> Type.is_union l && is_coercible_to_member_type_of_union l r) ltype rhs
    then
      rhs
    else
      die (Expression_error ("cannot assign to incompatible struct/union", None, [rhs]))


  (* default error message *)
  else
    die (Type_error ("operands have incompatible types", None, [ltype; rtype]))


and is_coercible_to_member_type_of_union union rhs =
  List.exists (fun memty ->
    try
      ignore (coerce true true rhs memty); true
    with Not_found ->
      false
  ) (Sue.member_types union)


let assignment = coerce false false


let return = assignment
