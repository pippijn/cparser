open Ast

let empty = {
  d = EmptyDecl;
  d_sloc = Location.dummy;
}


let rec add_tqual decl tqs =
  { decl with
    d =
      match decl.d with
      | StructDeclarator (sdecl, bitfield) ->
          StructDeclarator (add_tqual sdecl tqs, bitfield)

      | TypedDecl (scope, sclasses, untyped, decl, asm, init) ->
          TypedDecl (scope, sclasses, Types.add_tqual untyped tqs, decl, asm, init)

      | _ -> die (Declaration_error ("add_tqual_to_decl", None, [decl]))
  }


let rec add_basic_type decl bt =
  { decl with
    d =
      match decl.d with
      | TypedDecl (scope, sclasses, ty, untyped, asm, init) ->
          TypedDecl (scope, sclasses, Types.add_basic_type ty bt, untyped, asm, init)

      | _ -> die (Declaration_error ("add_basic_type", None, [decl]))
  }


let rec add_dqual decl dq =
  { decl with
    d =
      match decl.d with
      | TypedDecl (scope, sclasses, ty, decl, asm, init) ->
          begin match dq with
          | Some sclass, None ->
              TypedDecl (scope, Sclass.add sclasses sclass, ty, decl, asm, init)
          | None, Some tqual ->
              TypedDecl (scope, sclasses, Types.add_tqual ty [tqual], decl, asm, init)
          | _ -> die (Declaration_error ("add_dqual", None, [decl]))
          end
      | _ -> die (Declaration_error ("add_dqual", None, [decl]))
  }


let rec add_pointer_type decl =
  { decl with
    d =
      match decl.d with
      | WildcardDecl _
      | IdentifierDeclarator _ ->
          TypedDecl ("",
                     Sclass.empty,
                     PointerType (EmptyType),
                     decl,
                     empty,
                     None)
      | TypedDecl (scope, sclasses, ty, decl, asm, init) ->
          TypedDecl (scope, sclasses, PointerType (ty), decl, asm, init)
      | _ -> die (Declaration_error ("add_pointer_type", None, [decl]))
  }


let rec decl_type decl =
  match decl.d with
  | TypedDecl (_, _, ty, _, _, _) -> ty
  | FunctionDefinition (decl, _) -> decl_type decl
  | _ -> die (Declaration_error ("unexpected declaration kind in decl_type", None, [decl]))


let rec decl_decl decl =
  match decl.d with
  | TypedDecl (_, _, _, decl, _, _) -> decl
  | _ -> die (Declaration_error ("unexpected declaration kind in decl_decl", None, [decl]))


let rec decl_name decl =
  match decl.d with
  | WildcardDecl (id)
  | IdentifierDeclarator (_, id) ->
      id
  | TypedDecl (_, _, _, decl, _, _) ->
      decl_name decl
  | _ -> die (Declaration_error ("unexpected type in decl_name", None, [decl]))


let rec get_parameter_types_from_list table = function
  | hd :: tl ->
      Hashtbl.add table (decl_name hd) (decl_type hd);
      get_parameter_types_from_list table tl
  | [] -> table

let rec get_parameter_types table = function
  | hd :: tl ->
      let table =
        match hd.d with
        | DeclaringList (decls) ->
            get_parameter_types_from_list table decls
        | _ ->
            die (Declaration_error (
                  "unexpected type in get_parameter_types", None, [hd]))
      in
      get_parameter_types table tl
  | [] -> table


let add_parameter_types fdecl decls =
  let types = get_parameter_types (Hashtbl.create (List.length decls)) decls in
  let rec add decl =
    { decl with
      d =
        match decl.d with
        | TypedDecl (scope, sclasses,
                     (PartialBasicType [BT_Default] as default_int),
                     decl, asm, init) ->
            let ty = try
                Hashtbl.find types (decl_name decl)
              with
              | Not_found -> default_int
            in
            TypedDecl (scope, sclasses, ty, decl, asm, init)
        | _ ->
            die (Declaration_error (
                "add_parameter_types.add", None, [decl]))
    }
  in
  { fdecl with
    d =
      match fdecl.d with
      | TypedDecl (scope, sclasses,
                   FunctionType (retty, params),
                   decl, asm, init) ->
          TypedDecl (scope, sclasses,
                     FunctionType (retty, List.map add params),
                     decl, asm, init)
      | _ -> die (Declaration_error ("add_parameter_types", None, [fdecl]))
  }


let rec set_base_type decl ty =
  { decl with
    d =
      match decl.d with
      | EmptyDecl ->
          TypedDecl ("", Sclass.empty, ty, empty, empty, None)
      | WildcardDecl _
      | IdentifierDeclarator _ ->
          TypedDecl ("", Sclass.empty, ty, decl, empty, None)
      | TypedDecl (scope, sclasses, declty, untyped, asm, init) ->
          TypedDecl (scope, sclasses, Types.set_base_type ty declty, untyped, asm, init)
      | _ -> die (Declaration_error ("set_base_type", None, [decl]))
  }


let rec set_tspec decl spec =
  { decl with
    d =
      match decl.d with
      | EmptyDecl ->
          TypedDecl ("", Sclass.empty, spec, decl, empty, None)
      | WildcardDecl _
      | IdentifierDeclarator _ ->
          TypedDecl ("", Sclass.empty, spec, decl, empty, None)
      | StructDeclarator (sdecl, bitfield) ->
          StructDeclarator (set_tspec sdecl spec, bitfield)
      | TypedDecl (scope, sc, declty, untyped, { d = EmptyDecl }, None) when Sclass.is_empty sc ->
          TypedDecl (scope, Sclass.empty, Types.set_base_type spec declty, untyped, empty, None)
      | _ -> die (Declaration_error ("set_tspec", None, [decl]))
  }


let rec merge_decls spec decl =
  match spec.d with
  | TypedDecl (scope, sclasses, specty,
               { d = IdentifierDeclarator (attrs, "") }, asm, init) ->
      Traits.add_attrs [attrs] (
        merge_decls
          { spec with
            d = TypedDecl (scope, sclasses, specty, empty, asm, init) }
          decl
      )
  | TypedDecl (scope, sclasses, specty, { d = EmptyDecl }, asm, init) ->
      { spec with
        d =
          match decl.d with
          | TypedDecl (_, sc, declty, untyped, { d = EmptyDecl }, None) when Sclass.is_empty sc ->
              TypedDecl (scope, sclasses, Types.set_base_type specty declty, untyped, asm, init)
          | StructDeclarator (sdecl, bitfield) ->
              StructDeclarator (merge_decls spec sdecl, bitfield)
          | WildcardDecl _
          | IdentifierDeclarator _ ->
              TypedDecl (scope, sclasses, specty, decl, asm, init)
          | _ -> die (Declaration_error ("merge_decls invalid decl", None, [decl]))
      }
  | _ -> die (Declaration_error ("merge_decls invalid spec", None, [spec]))


let rec decl_base_type decl =
  match decl.d with
  | TypedDecl (scope, sclasses, declty, decl, asm, init) ->
      { decl with
        d = TypedDecl (scope, sclasses,
                       Types.base_type declty,
                       empty,
                       empty,
                       None)
      }
  | StructDeclarator (sdecl, _) ->
      decl_base_type sdecl
  | _ -> die (Declaration_error ("unexpected type in decl_base_type", None, [decl]))


let finish_decl decl asm init =
  { decl with
    d =
      match decl.d with
      | TypedDecl (scope, sclasses, declty, decl, { d = EmptyDecl }, None) ->
          let register =
            if Sclass.is_typedef sclasses then
              Lexer_hack.typedef
            else
              Lexer_hack.identifier
          in
          register (decl_name decl);
          TypedDecl (scope, sclasses, declty, decl, asm, init)
      | _ -> die (Declaration_error ("unexpected decl in finish_decl", None, [decl]))
  }


let return_type = function
  | { d = TypedDecl (_, _, FunctionType (retty, _), _, _, _) } -> retty
  | decl -> die (Declaration_error ("declaration is not a function", None, [decl]))


let enter_function = function
  | { d = TypedDecl (_, _, FunctionType (_, args), _, _, _) } ->
      Lexer_hack.push_scope ();
      (* Register all argument names as identifiers in the function's scope. *)
      List.iter (function
        | { d = TypedDecl (_, _, _, { d = EmptyDecl }, _, _) } ->
            (* void or ... *)
            ()
        | arg ->
            Lexer_hack.identifier (decl_name arg)
      ) args

  | decl -> die (Declaration_error ("declaration is not a function", None, [decl]))


let leave_function = Lexer_hack.pop_scope
