open Ast

let empty_position = [`Position Location.dummy]

let opt = Option.map


(* {{{ *)
let traits_of_type = function
  | EmptyType -> empty_position

  (* Wildcards *)
  | WildcardType (trs, _) -> trs

  (* Types *)
  | PartialBasicType _
  | BasicType _
  | QualifiedType _
  | PointerType _
  | SUEType _
  | TypedefType _
  | ArrayType _
  | FunctionType _
  | TypeofType _
  | TypeofExpr _ -> empty_position
(* }}} *)


let pos_of_type ty =
  try
    Attributes.position (traits_of_type ty)
  with Option.No_value ->
    die (Type_error ("no position", None, [ty]))


let rec add_attrs atts decl =
  match atts with
  | [] -> decl
  | attrs ->
      { decl with
        d =
          match decl.d with
          | EmptyDecl ->
              IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) [], "")
          | IdentifierDeclarator (trs, id) ->
              IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) trs, id)
          | StructDeclarator (decl, bitfield) ->
              StructDeclarator (add_attrs attrs decl, bitfield)
          | TypedDecl (scope, sc, ty, decl, asm, init) ->
              TypedDecl (scope, sc, ty, add_attrs attrs decl, asm, init)
          | DeclaringList (decl :: tl) ->
              DeclaringList (add_attrs attrs decl :: tl)
          | FunctionDefinition (decl, body) ->
              FunctionDefinition (add_attrs attrs decl, body)
          | _ -> die (Declaration_error ("invalid declaration", None, [decl]))
      }
