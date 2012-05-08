open Ast


let rec add_tqual decl tqs =
  match decl with
  | StructDeclarator (trs, sdecl, bitfield) ->
      StructDeclarator (trs, add_tqual sdecl tqs, bitfield)
      
  | TypedDecl (trs, sclasses, untyped, decl, asm, init) ->
      TypedDecl (trs, sclasses, Types.add_tqual untyped tqs, decl, asm, init)

  | _ -> die (Declaration_error ("add_tqual_to_decl", None, [decl]))


let rec add_basic_type decl bt =
  match decl with
  | TypedDecl (trs, sclasses, ty, untyped, asm, init) ->
      TypedDecl (trs, sclasses, Types.add_basic_type ty bt, untyped, asm, init)

  | decl -> die (Declaration_error ("add_basic_type", None, [decl]))


let rec add_dqual decl dq =
  match decl with
  | TypedDecl (trs, sclasses, ty, decl, asm, init) ->
      begin match dq with
      | Some sclass, None ->
          TypedDecl (trs, Sclass.add sclasses sclass, ty, decl, asm, init)
      | None, Some tqual ->
          TypedDecl (trs, sclasses, Types.add_tqual ty [tqual], decl, asm, init)
      | _ -> die (Declaration_error ("add_dqual", None, [decl]))
      end
  | _ -> die (Declaration_error ("add_dqual", None, [decl]))


let rec add_pointer_type decl =
  match decl with
  | WildcardDecl _
  | IdentifierDeclarator _ ->
      TypedDecl (Traits.copy_pos decl, Sclass.empty, PointerType (NoType), decl, Nothing, None)
  | TypedDecl (trs, sclasses, ty, decl, asm, init) ->
      TypedDecl (trs, sclasses, PointerType (ty), decl, asm, init)
  | _ -> die (Declaration_error ("add_pointer_type", None, [decl]))


let rec decl_type = function
  | TypedDecl (_, _, ty, _, _, _) -> ty
  | FunctionDefinition (_, decl, _) -> decl_type decl
  | decl -> die (Declaration_error ("unexpected declaration kind in decl_type", None, [decl]))


let rec decl_decl = function
  | TypedDecl (_, _, _, decl, _, _) -> decl
  | decl -> die (Declaration_error ("unexpected declaration kind in decl_decl", None, [decl]))


let rec decl_name = function
  | WildcardDecl (_, id)
  | IdentifierDeclarator (_, id) ->
      id
  | TypedDecl (_, _, _, decl, _, _) ->
      decl_name decl
  | decl -> die (Declaration_error ("unexpected type in decl_name", None, [decl]))


let rec get_parameter_types_from_list table = function
  | hd :: tl ->
      Hashtbl.add table (decl_name hd) (decl_type hd);
      get_parameter_types_from_list table tl
  | [] -> table

let rec get_parameter_types table = function
  | hd :: tl ->
      let table = begin match hd with
      | DeclaringList (_, decls) ->
          get_parameter_types_from_list table decls
      | decl -> die (Declaration_error ("unexpected type in get_parameter_types", None, [decl]))
      end in
      get_parameter_types table tl
  | [] -> table


let add_parameter_types fdecl decls =
  let types = get_parameter_types (Hashtbl.create (List.length decls)) decls in
  let rec add decl =
    match decl with
    | TypedDecl (trs, sclasses, (PartialBasicType [BT_Default] as default_int), decl, asm, init) ->
        let ty = try
          Hashtbl.find types (decl_name decl)
        with
        | Not_found -> default_int
        in
        TypedDecl (trs, sclasses, ty, decl, asm, init)
    | decl -> die (Declaration_error ("add_parameter_types.add", None, [decl]))
  in
  match fdecl with
  | TypedDecl (trs, sclasses, FunctionType (retty, params), decl, asm, init) ->
      TypedDecl (trs, sclasses, FunctionType (retty, List.map add params), decl, asm, init)
  | _ -> die (Declaration_error ("add_parameter_types", None, [fdecl]))


let rec set_base_type decl ty =
  match decl with
  | Nothing ->
      TypedDecl ([], Sclass.empty, ty, Nothing, Nothing, None)
  | WildcardDecl _
  | IdentifierDeclarator _ as decl ->
      TypedDecl (Traits.copy_pos decl, Sclass.empty, ty, decl, Nothing, None)
  | TypedDecl (trs, sclasses, declty, untyped, asm, init) ->
      TypedDecl (trs, sclasses, Types.set_base_type ty declty, untyped, asm, init)
  | decl -> die (Declaration_error ("set_base_type", None, [decl]))


let rec set_tspec decl spec =
  match decl with
  | Nothing ->
      TypedDecl ([], Sclass.empty, spec, decl, Nothing, None)
  | WildcardDecl _
  | IdentifierDeclarator _ ->
      TypedDecl (Traits.copy_pos decl, Sclass.empty, spec, decl, Nothing, None)
  | StructDeclarator (trs, sdecl, bitfield) ->
      StructDeclarator (trs, set_tspec sdecl spec, bitfield)
  | TypedDecl (trs, sc, declty, untyped, Nothing, None) when Sclass.is_empty sc ->
      TypedDecl (trs, Sclass.empty, Types.set_base_type spec declty, untyped, Nothing, None)
  | _ -> die (Declaration_error ("set_tspec", None, [decl]))


let rec merge_decls spec decl =
  match spec with
  | TypedDecl (trs, sclasses, specty, IdentifierDeclarator (idtrs, ""), asm, init) ->
      Traits.add_attrs [Attributes.attribute idtrs] (merge_decls (TypedDecl (trs, sclasses, specty, Nothing, asm, init)) decl)
  | TypedDecl (trs, sclasses, specty, Nothing, asm, init) ->
      begin match decl with
      | TypedDecl (_, sc, declty, untyped, Nothing, None) when Sclass.is_empty sc ->
          TypedDecl (trs, sclasses, Types.set_base_type specty declty, untyped, asm, init)
      | StructDeclarator (trs, sdecl, bitfield) ->
          StructDeclarator (trs, merge_decls spec sdecl, bitfield)
      | WildcardDecl _
      | IdentifierDeclarator _ ->
          TypedDecl (trs, sclasses, specty, decl, asm, init)
      | _ -> die (Declaration_error ("merge_decls invalid decl", None, [decl]))
      end
  | _ -> die (Declaration_error ("merge_decls invalid spec", None, [spec]))


let rec decl_base_type decl =
  match decl with
  | TypedDecl (trs, sclasses, declty, decl, asm, init) ->
      TypedDecl (trs, sclasses, Types.base_type declty, Nothing, Nothing, None)
  | StructDeclarator (_, sdecl, _) ->
      decl_base_type sdecl
  | _ -> die (Declaration_error ("unexpected type in decl_base_type", None, [decl]))


let finish_decl decl asm init =
  match decl with
  | TypedDecl (trs, sclasses, declty, decl, Nothing, None) ->
      let register =
        if Sclass.is_typedef sclasses then
          Lexer_hack.typedef
        else
          Lexer_hack.identifier
      in
      register (decl_name decl);
      TypedDecl (trs, sclasses, declty, decl, asm, init)
  | _ -> die (Declaration_error ("unexpected decl in finish_decl", None, [decl]))


let return_type = function
  | TypedDecl (_, _, FunctionType (retty, _), _, _, _) -> retty
  | decl -> die (Declaration_error ("declaration is not a function", None, [decl]))
