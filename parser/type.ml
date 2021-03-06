open Ast
open CorePervasives
open Tqual


(****************************************************************************
 *
 * Helper functions
 *
 ****************************************************************************)


let is_integral_basic_type = function
  | Bool
  | Char
  | WCharT
  | SChar
  | UChar
  | SShort
  | UShort
  | SInt
  | UInt
  | SLong
  | ULong
  | SLongLong
  | ULongLong
  | SIntN _
  | UIntN _ -> true
  | _ -> false


let is_floating_basic_type = function
  | Float
  | Double
  | LongDouble
  | DecimalN _
  | FloatN _ -> true
  | _ -> false


let is_arithmetic_basic_type bt =
  is_integral_basic_type bt ||
  is_floating_basic_type bt


let is_integral = function
  | BasicType bt -> is_integral_basic_type bt
  | _ -> false


let is_floating = function
  | BasicType bt -> is_floating_basic_type bt
  | _ -> false


let is_arithmetic = function
  | BasicType bt -> is_arithmetic_basic_type bt
  | _ -> false


let is_pointer = function
  | PointerType _ -> true
  | _ -> false


let is_unsized_array = function
  | ArrayType (None, _) -> true
  | _ -> false

let is_incomplete = function
  | SUEType (_, _, _, [])
  | BasicType Void ->
      true
  | ArrayType _ as ty when is_unsized_array ty ->
      true
  | _ ->
      false

let is_complete ty = not (is_incomplete ty)


let rec alignof ty =
  match ty.t with
  | ArrayType (_, base) ->
      alignof base

  | BasicType bt ->
      begin match bt with
      (* Integral types. *)
      | Bool
      | Char
      | SChar
      | UChar -> 1
      | SShort
      | UShort -> Platform.alignof_short
      | SInt
      | UInt -> Platform.alignof_int
      | SLong
      | ULong -> Platform.alignof_long
      | SLongLong
      | ULongLong -> Platform.alignof_long_long

      (* Floating point types. *)
      | Float -> Platform.alignof_float
      | Double -> Platform.alignof_double
      | LongDouble -> Platform.alignof_long_double

      (* XXX: assumes alignment = size for sized types *)

      (* Sized floating point types. *)
      | FloatN bits
      | DecimalN bits

      (* Sized integral types. *)
      | SIntN bits
      | UIntN bits ->
          let bits =
            let rest = bits mod Platform.char_bit in
            if rest = 0 then
              bits
            else
              bits + (Platform.char_bit - rest)
          in
          bits / Platform.char_bit

      (* Other built-in types *)
      | WCharT -> alignof (Platform.wchar_t)
      | VaList -> Platform.alignof_object_pointer (* FIXME: wrong! *)
      | Ellipsis -> die (Type_error ("variadic type `...' has no alignment", None, []))
      | Void -> die (Type_error ("cannot compute alignment of `void' type", None, []))
      end

  | PointerType { t = FunctionType _ } -> Platform.alignof_function_pointer
  | PointerType _ -> Platform.alignof_object_pointer

  | SUEType (_, _, _, _::_) ->
      let members = Sue.member_types ty.t in
      ExList.max (List.map alignof members)

  | _ -> die (Type_error ("cannot compute alignment of type", None, [ty]))


let rec sizeof ty =
  match ty.t with
  | ArrayType (Some arity, base) ->
      Constant.to_int arity.e_cval * sizeof base

  | BasicType bt ->
      begin match bt with
      (* Integral types. *)
      | Bool
      | Char
      | SChar
      | UChar -> 1
      | SShort
      | UShort -> Platform.sizeof_short
      | SInt
      | UInt -> Platform.sizeof_int
      | SLong
      | ULong -> Platform.sizeof_long
      | SLongLong
      | ULongLong -> Platform.sizeof_long_long

      (* Floating point types. *)
      | Float -> Platform.sizeof_float
      | Double -> Platform.sizeof_double
      | LongDouble -> Platform.sizeof_long_double

      (* Sized floating point types. *)
      | FloatN bits
      | DecimalN bits

      (* Sized integral types. *)
      | SIntN bits
      | UIntN bits ->
          let bits =
            let rest = bits mod Platform.char_bit in
            if rest = 0 then
              bits
            else
              bits + (Platform.char_bit - rest)
          in
          bits / Platform.char_bit

      (* Other built-in types *)
      | WCharT -> sizeof (Platform.wchar_t)
      | VaList -> Platform.sizeof_object_pointer (* FIXME: wrong! *)
      | Ellipsis -> die (Type_error ("variadic type `...' has no size", None, []))
      | Void -> die (Type_error ("cannot compute size of `void' type", None, []))
      end

  | PointerType { t = FunctionType _ } -> Platform.sizeof_function_pointer
  | PointerType _ -> Platform.sizeof_object_pointer

  | SUEType (_, _, _, [{ d = EmptyDecl }]) ->
      (* Empty structs are still 1 byte large. *)
      1

  | SUEType (_, SUE_Union, _, _::_) ->
      Sue.member_types ty.t
      |> List.map sizeof
      |> ExList.max

  (* TODO: support packed structs. *)
  | SUEType (_, SUE_Struct, _, _::_) ->
      let members = Sue.member_types ty.t in

      let align size alignment =
        if size mod alignment <> 0 then
          size + (alignment - size mod alignment)
        else
          size
      in

      (* Calculate size. *)
      let size =
        List.fold_left (fun size membersize ->
          (* Padding for next member. *)
          let size = align size membersize in
          size + membersize
        ) 0 (List.map sizeof members)
      in

      (* Alignment for struct. *)
      let alignment = alignof ty in
      align size alignment

  | _ -> die (Type_error ("cannot compute size of type", None, [ty]))


let rec resolve ty =
  match ty.t with
  | TypeofExpr (expr) -> resolve expr.e_type
  | TypeofType (ty) -> resolve ty
  | BasicType _ -> ty
  | ArrayType (arity, base) ->
      { ty with t = ArrayType (arity, resolve base) }
  | PointerType (base) ->
      { ty with t = PointerType (resolve base) }
  | QualifiedType (tqs, base) ->
      { ty with t = QualifiedType (tqs, resolve base) }
  | TypedefType (name) ->
      let typedef = Csymtab.lookup_decl name Symtab.Ordinary in
      resolve (Decls.decl_type typedef)
  | FunctionType (retty, params) ->
      { ty with
        t = FunctionType (resolve retty, List.map resolve_decl params) }
  | SUEType (_, _, tag, []) ->
      begin try
          match Csymtab.lookup_decl tag Symtab.Tag with
          | { d = TypedDecl (_, _, ({ t = SUEType _ } as ty), _, _, _) } -> ty
          | decl -> die (Declaration_error ("struct/union/enum tag resolved to non-sue type", None, [decl]))
        with Not_found ->
          ty
      end
  | SUEType (_, _, tag, _) -> ty
  | _ -> die (Type_error ("cannot resolve type", None, [ty]))

and resolve_decl decl =
  { decl with
    d =
      match decl.d with
      | TypedDecl (trs, sc, ty, untyped, asm, init) ->
          TypedDecl (trs, sc, resolve ty, untyped, asm, init)
      | _ -> die (Declaration_error ("cannot resolve declaration type", None, [decl]))
  }


let rec is_lvalue_ty modifiablep ty =
  match ty.t with
  | QualifiedType (tq, _) when modifiablep && Tqual.is_const tq -> false

  | QualifiedType (_, base) -> is_lvalue_ty modifiablep base

  | BasicType _ -> true
  | PointerType _ -> true
  | SUEType _ -> true

  | ArrayType _ -> false
  | FunctionType _ -> false

  | TypeofExpr _
  | TypeofType _
  | TypedefType _ -> die (Type_error ("is_lvalue_ty expects resolved type", None, [ty]))

  | _ -> die (Type_error ("unhandled type in is_lvalue_ty", None, [ty]))


let rec is_lvalue modifiablep expr =
  (* test first for modifiability, if required by caller *)
  is_lvalue_ty modifiablep expr.e_type &&

  match expr.e with
  (* *p is an lvalue. *)
  | UnaryExpression (OP_Dereference, _)
  (* a[i] is *(a + i), thus an lvalue. *)
  | ArrayAccess _
  (* Whether the identifier is an lvalue is known from its type. *)
  | Identifier _ -> true

  | IntegerLiteral _
  | FloatingLiteral _ -> false

  | _ -> die (Expression_error ("unhandled in is_lvalue", None, [expr]))
  (*| QualifiedType (tqs, _) when modifiablep && List.mem TQ_Const tqs ->*)
      (*false*)

  (*| QualifiedType (_, base) -> is_lvalue modifiablep base*)


let is_modifiable_lvalue = is_lvalue true
let is_lvalue = is_lvalue false


let is_void = function
  | BasicType Void -> true
  | _ -> false


let is_union = function
  | SUEType (_, SUE_Union, _, _) -> true
  | _ -> false

let is_struct = function
  | SUEType (_, SUE_Struct, _, _) -> true
  | _ -> false

let is_aggregate = function
  | SUEType (_, (SUE_Struct | SUE_Union), _, _) -> true
  | _ -> false


(*
 * implements type equivalence according to K&R2 section
 * A8.10 (TODO: what section in ANSI standard?)
 *
 * strict_toplevel and strict_recursive control whether
 * const and volatile (and other type qualifiers specified
 * in Tqual.compatible) are ignored:
 *
 * !strict_toplevel  => type quals are ignored when comparing
 *                      roots of type1 and type2
 * !strict_recursive => type quals are ignored when comparing
 *                      children of type1 and type2
 *
 *)
let rec equal_qualified strict_toplevel strict_recursive ty1 ty2 =
  let equal = equal_qualified strict_recursive strict_recursive in

  match ty1, ty2 with
  | QualifiedType (tqs1, base1), QualifiedType (tqs2, base2) ->
      let tqs1, tqs2 =
        if not strict_toplevel then
          Tqual.remove_compatible tqs1,
          Tqual.remove_compatible tqs2
        else
          tqs1, tqs2
      in

      tqs1 = tqs2 &&
      equal base1.t base2.t

  | BasicType bt1, BasicType bt2 ->
      bt1 = bt2

  | PointerType base1, PointerType base2 ->
      equal base1.t base2.t

  | ArrayType (arity1, base1), ArrayType (arity2, base2) ->
      equal base1.t base2.t &&
      begin match arity1, arity2 with
      | Some arity1, Some arity2 ->
          (* If both dims are specified they must be the same *)
          (* TODO: get constant int value *)
          arity1 = arity2
      | _ ->
          true
      end

  | SUEType (_, _, tag1, _), SUEType (_, _, tag2, _) ->
      tag1 = tag2

  | FunctionType (retty1, params1), FunctionType (retty2, params2) ->
      equal retty1.t retty2.t &&
      begin match params1, params2 with
      (* if either list is "unspecified" assume comparison is successful *)
      | [], _
      | _, [] -> true

      | l1, l2 when List.length l1 <> List.length l2 -> false

      | l1, l2 ->
          List.for_all2 (fun p1 p2 ->
            let p1 = Decls.decl_type p1 in
            let p2 = Decls.decl_type p2 in
            equal p1.t p2.t
          ) l1 l2
      end

  | ty, TypeofExpr expr
  | TypeofExpr expr, ty ->
      equal expr.e_type.t ty

  | ty, TypeofType tty
  | TypeofType tty, ty ->
      equal tty.t ty

  | PartialBasicType _, PartialBasicType _ -> failwith "incomplete basic type"
  | TypedefType _, TypedefType _ -> failwith "unresolved typedef type cannot be compared"

  | ty1, ty2 ->
      false


let equal = equal_qualified false false


(* sans_sign folds signed types into unsigned types of the same width. *)
let sans_sign ty =
  { ty with
    t =
      match ty.t with
      | BasicType bt ->
          BasicType (
            match bt with
            | UChar
            | SChar
            | Char -> Char

            | UShort
            | SShort -> UShort

            | UInt
            | SInt -> UInt

            | ULong
            | SLong -> ULong

            | ULongLong
            | SLongLong -> ULongLong

            | UIntN bits
            | SIntN bits -> UIntN bits

            (* Keep the other types as-is. *)
            | Bool
            | Float
            | Double
            | LongDouble

            | FloatN _
            | DecimalN _

            | WCharT
            | VaList
            | Ellipsis
            | Void as bt ->
                bt
          )

      | ty -> ty
  }


let rec pointer_base ty =
  match ty.t with
  | PointerType (base) ->
      base

  | QualifiedType (_, ty) ->
      pointer_base ty

  | _ -> die (Type_error ("unexpected type in pointer_base", None, [ty]))


let is_function = function
  | FunctionType _ -> true
  | _ -> false


let is_object ty =
  not (is_function ty || is_incomplete ty)


let is_scalar ty =
  is_arithmetic ty || is_pointer ty


let is_array = function
  | ArrayType _ -> true
  | _ -> false
