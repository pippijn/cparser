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


let traits_of_decl = function
  | EmptyDecl
  | TranslationUnit (_) -> []

  (* Wildcards *)
  | WildcardDecl (trs, _)

  (* #include etc. *)
  | PreprocessorDirective (trs, _)

  (* Syntax errors *)
  | SyntaxError (trs, _, _)

  (* Generic nodes *)
  | AsmSpecifier (trs, _)
  | FunctionDefinition (trs, _, _)
  | StructDeclarator (trs, _, _)
  | DeclaringList (trs, _)

  (* Statements *)
  | ToplevelAsm (trs, _)

  (* Struct/union/enum types *)
  | Enumerator (trs, _, _) -> trs

  | TypedDecl (trs, _, _, _, _, _) ->
      (* Ignore scope. *)
      begin match Attributes.position_opt trs with
      | None -> []
      | Some pos -> Attributes.set_position pos []
      end
  | IdentifierDeclarator (trs, _) ->
      (* Ignore attributes. *)
      begin match Attributes.position_opt trs with
      | None -> []
      | Some pos -> Attributes.set_position pos []
      end


(* }}} *)


let copy_pos node =
  let trs = traits_of_decl node in
  Attributes.set_position (Attributes.position trs) []


let set_pos_decl pos = function
  | TypedDecl (trs, sclasses, ty, untyped, asm, init) ->
      TypedDecl (Attributes.set_position pos trs, sclasses, ty, untyped, asm, init)
  | node -> node


let pos_of_type ty =
  try
    Attributes.position (traits_of_type ty)
  with Option.No_value ->
    die (Type_error ("no position", None, [ty]))


let pos_of_decl decl =
  Option.default Location.dummy (Attributes.position_opt (traits_of_decl decl))


let rec add_attrs atts decl =
  match atts with
  | [] -> decl
  | attrs ->
      match decl with
      | EmptyDecl ->
          IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) [], "")
      | IdentifierDeclarator (trs, id) ->
          IdentifierDeclarator (Attributes.add_attribute (List.flatten attrs) trs, id)
      | StructDeclarator (trs, decl, bitfield) ->
          StructDeclarator (trs, add_attrs attrs decl, bitfield)
      | TypedDecl (trs, sc, ty, decl, asm, init) ->
          TypedDecl (trs, sc, ty, add_attrs attrs decl, asm, init)
      | DeclaringList (trs, decl :: tl) ->
          DeclaringList (trs, add_attrs attrs decl :: tl)
      | FunctionDefinition (trs, decl, body) ->
          FunctionDefinition (trs, add_attrs attrs decl, body)
      | decl -> die (Declaration_error ("invalid declaration", None, [decl]))
