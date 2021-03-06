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
    e_type =
      { expr.e_type with
        t =
          match expr.e_type.t with
          (* Convert char and short to int, keeping their signedness. *)
          | BasicType (SChar | SShort) -> BasicType SInt
          | BasicType (UChar | UShort) -> BasicType UInt
          | BasicType Char ->
              if Platform.char_is_signed then
                BasicType SInt
              else
                BasicType UInt
          (* Convert arrays to pointers. *)
          | ArrayType (_, base) -> PointerType base
          (* Convert functions to pointers to functions. *)
          | FunctionType _ -> PointerType expr.e_type
          | _ -> expr.e_type.t
      }
  }


let binary a b =
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
    | bt ->
        die (Type_error ("invalid basic type", None,
                         [{ t = BasicType bt;
                            t_sloc = Location.dummy;
                          }]))
  in

  let convert to_ty expr =
    (* Convert value to floating point, if the target type is a
     * floating point type. *)
    { expr with
      e_cval = 
        if Type.is_floating to_ty.t && not (Type.is_floating expr.e_type.t) then
          let open Constant in
          match expr.e_cval with
          | IntValue i ->
              FloatValue (Mach_int.float_of_mach_int i)
          | _ -> failwith "conversion"
        else
          expr.e_cval;
      e_type = to_ty;
    }
  in

  begin match a.e_type.t, b.e_type.t with
  (* If the types are already equal, no conversion is required. *)
  | lty, rty when Type.equal lty rty -> a, b

  (* For basic types, convert the smaller type to the bigger type. *)
  | BasicType lbt, BasicType rbt ->
      if rank lbt < rank rbt then
        (* Convert left expression to right type. *)
        convert b.e_type a, b
      else
        (* Convert right expression to left type. *)
        a, convert a.e_type b

  (* Complex types undergo no conversions. *)
  | _ ->
      a, b
  end


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
    if f l.t r.t then
      die (Type_error (msg, None, [l; r]))
    else
      false
  in

  let expr_error_if msg f l r =
    if f l.t r then
      die (Expression_error (msg, None, [r]))
    else
      false
  in

  let rtype = rhs.e_type in


  if Type.is_arithmetic rtype.t && Type.is_arithmetic ltype.t then
    rhs

  else if Type.is_pointer rtype.t && Type.is_pointer ltype.t then
    if ptr2ptrp then
      rhs
    else
      let lbasetype = Type.pointer_base ltype in
      let rbasetype = Type.pointer_base rtype in

      let assignable =
        (* Valid conversions. *)
        Type.equal lbasetype.t rbasetype.t ||
        Type.is_void lbasetype.t && Type.is_void rbasetype.t ||
        type_assignable_modulo_cv lbasetype.t rbasetype.t ||

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


  else if Type.is_integral rtype.t && Type.is_pointer ltype.t then
    if ptr2intp || Const_eval.is_zero rhs then
      rhs
    else
      die (Expression_error ("assignment of integer value to pointer", None, [rhs]))


  else if Type.is_pointer ltype.t && Type.is_integral rtype.t then
    if ptr2intp then
      rhs
    else
      die (Expression_error ("assignment of pointer value to integer", None, [rhs]))


  else if Type.is_aggregate ltype.t then
    if
      Type.equal rtype.t ltype.t ||
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
      ignore (coerce true true rhs memty);
      true
    with Not_found ->
      false
  ) (Sue.member_types union)


let assignment = coerce false false


let return = assignment
