open Ast

(* TODO: merge into Type module *)


let add_tqual ty = function
  | [] -> ty
  | tqs ->
      let rec add = function
        | EmptyType
        | PartialBasicType _
        | BasicType _
        | PointerType _
        | TypedefType _
        | TypeofExpr _
        | TypeofType _
        | SUEType _ ->
            QualifiedType (Tqual.add Tqual.empty tqs, ty)

        | QualifiedType (quals, ty) ->
            QualifiedType (Tqual.add quals tqs, ty)

        | _ -> die (Type_error ("add_tqual_to_ty", None, [ty]))
      in
      add ty


let rec add_basic_type ty bt =
  match ty with
  | EmptyType ->
      PartialBasicType ([bt])

  | PartialBasicType (bts) ->
      PartialBasicType (bt :: bts)

  | QualifiedType (tqs, unqual) ->
      QualifiedType (tqs, add_basic_type unqual bt)

  | _ -> die (Type_error ("add_basic_type", None, [ty]))


let rec pointer_to ty =
  match ty with
  | EmptyType ->
      PointerType (EmptyType)
  | ArrayType (arity, base) ->
      ArrayType (arity, pointer_to base)
  | _ -> die (Type_error ("pointer_ty", None, [ty]))


let rec set_base_type newbase =
  let set ty = set_base_type newbase ty in function
  | EmptyType ->
      newbase
  | ArrayType (arity, base) ->
      ArrayType (arity, set base)
  | PointerType (pointee) ->
      PointerType (set pointee)
  | FunctionType (retty, params) ->
      FunctionType (set retty, params)
  | QualifiedType (tquals, unqual) ->
      QualifiedType (tquals, set unqual)

  | ty -> die (Type_error ("set_base_type", None, [ty]))


let make_array_type arrays =
  List.fold_left (fun ty base -> set_base_type base ty) EmptyType arrays


let rec base_type ty =
  match ty with

  |                    TypeofExpr _ | TypeofType _ | PartialBasicType _ | BasicType _ | TypedefType _ | SUEType _
  | QualifiedType (_, (TypeofExpr _ | TypeofType _ | PartialBasicType _ | BasicType _ | TypedefType _ | SUEType _)) ->
      ty

  |                    ArrayType (_, base) | PointerType (base) | FunctionType (base, _)
  | QualifiedType (_, (ArrayType (_, base) | PointerType (base) | FunctionType (base, _))) ->
      base_type base

  | ty -> die (Type_error ("unexpected type in base_type", None, [ty]))


let rec unbase_type = function
  |                    TypeofExpr _ | TypeofType _ | PartialBasicType _ | BasicType _ | TypedefType _ | SUEType _
  | QualifiedType (_, (TypeofExpr _ | TypeofType _ | PartialBasicType _ | BasicType _ | TypedefType _ | SUEType _)) ->
      EmptyType

  |                     ArrayType (arity, base)		->                     ArrayType (arity, unbase_type base)
  | QualifiedType (tqs, ArrayType (arity, base))	-> QualifiedType (tqs, ArrayType (arity, unbase_type base))
  |                     FunctionType (retty, params)	->                     FunctionType (unbase_type retty, params)
  | QualifiedType (tqs, FunctionType (retty, params))	-> QualifiedType (tqs, FunctionType (unbase_type retty, params))
  |                     PointerType (base)		->                     PointerType (unbase_type base)
  | QualifiedType (tqs, PointerType (base))		-> QualifiedType (tqs, PointerType (unbase_type base))

  | ty -> die (Type_error ("unexpected type in unbase_type", None, [ty]))
