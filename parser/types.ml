open Ast

(* TODO: merge into Type module *)

let empty_ptr = {
  t = PointerType {
      t = EmptyType;
      t_sloc = Location.dummy;
    };
  t_sloc = Location.dummy;
}

let add_tqual ty = function
  | [] -> ty
  | tqs ->
      let rec add ty =
        { ty with
          t =
            match ty.t with
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
        }
      in
      add ty


let rec add_basic_type ty bt =
  { ty with
    t =
      match ty.t with
      | EmptyType ->
          PartialBasicType ([bt])

      | PartialBasicType (bts) ->
          PartialBasicType (bt :: bts)

      | QualifiedType (tqs, unqual) ->
          QualifiedType (tqs, add_basic_type unqual bt)

      | _ -> die (Type_error ("add_basic_type", None, [ty]))
  }


let rec pointer_to ty =
  { ty with
    t =
      match ty.t with
      | EmptyType ->
          PointerType ty
      | ArrayType (arity, base) ->
          ArrayType (arity, pointer_to base)
      | _ -> die (Type_error ("pointer_ty", None, [ty]))
  }


let rec set_base_type newbase ty =
  match ty.t with
  | EmptyType ->
      newbase
  | ArrayType (arity, base) ->
      { ty with t = ArrayType (arity, set_base_type newbase base) }
  | PointerType (pointee) ->
      { ty with t = PointerType (set_base_type newbase pointee) }
  | FunctionType (retty, params) ->
      { ty with t = FunctionType (set_base_type newbase retty, params) }
  | QualifiedType (tquals, unqual) ->
      { ty with t = QualifiedType (tquals, set_base_type newbase unqual) }

  | _ -> die (Type_error ("set_base_type", None, [ty]))


let make_array_type arrays =
  List.fold_left
    (fun ty base -> set_base_type base ty)
    { t = EmptyType;
      t_sloc = Location.dummy;
    }
    arrays


let rec base_type ty =
  match ty.t with

  | TypeofExpr _
  | TypeofType _
  | PartialBasicType _
  | BasicType _
  | TypedefType _
  | SUEType _ ->
      ty

  | ArrayType (_, base)
  | PointerType (base)
  | FunctionType (base, _) ->
      base_type base

  | QualifiedType (_, ty) ->
      base_type ty

  | _ -> die (Type_error ("unexpected type in base_type", None, [ty]))


let rec unbase_type ty =
  { ty with
    t =
      match ty.t with
      | TypeofExpr _
      | TypeofType _
      | PartialBasicType _
      | BasicType _
      | TypedefType _
      | SUEType _ ->
          EmptyType

      | ArrayType (arity, base) ->
          ArrayType (arity, unbase_type base)
      | FunctionType (retty, params) ->
          FunctionType (unbase_type retty, params)
      | PointerType (base) ->
          PointerType (unbase_type base)

      | QualifiedType (tqs, ty) ->
          begin match unbase_type ty with
          | { t = EmptyType } -> EmptyType
          | ty ->
              QualifiedType (tqs, ty)
          end

      | _ -> die (Type_error ("unexpected type in unbase_type", None, [ty]))
  }
