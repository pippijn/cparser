open Ast


let opt = Option.map


let normalise_unit =
  let anonymous prefix =
    let counter = ref 0 in
    let prefix = "__anon_" ^ prefix in
    function () ->
      counter := !counter + 1;
      prefix ^ (string_of_int !counter)
  in

  let anon_sue = anonymous "sue" in
  let anon_id  = anonymous "id" in

  let anon_decl () =
    { d = IdentifierDeclarator ([], anon_id ());
      d_sloc = Location.dummy;
    }
  in


  let rec normalise_struct = Visit.({
    map_type = normalise_type;
    map_decl = normalise_decl;
    map_stmt = normalise_stmt;
    map_expr = normalise_expr;
  })


  and normalise_decl decl =
    match decl.d with
    | IdentifierDeclarator (attrs, "") ->
        { decl with d = IdentifierDeclarator (attrs, anon_id ()) }
    | TypedDecl (scope, sc, ({ t = SUEType _ } as ty),
                 { d = EmptyDecl }, asm, init)
      when Sclass.is_typedef sc ->
        normalise_decl {
          decl with
          d = TypedDecl (scope, sc, ty, anon_decl (), asm, init)
        }
    | StructDeclarator (({ d = TypedDecl (scope, sc, ty,
                                          { d = EmptyDecl },
                                          asm, init)
                         } as inner),
                        bitfield) ->
        normalise_decl {
          decl with
          d = StructDeclarator ({
              inner with
              d = TypedDecl (scope, sc, ty, anon_decl (), asm, init)
            }, bitfield)
        }

    | _ -> Visit.map_decl normalise_struct decl


  and normalise_type ty =
    match ty.t with
    | PartialBasicType [BT_Default] ->
        { ty with t = BasicType SInt }

    | PartialBasicType bts ->
        { ty with t = BasicType (Basic_type.of_list bts) }

    (* XXX: not good
    | SUEType (attrs, kind, tag, [EmptyDecl]) ->
        let single_char_decl =
          DeclaringList ([], [
            StructDeclarator ([],
              TypedDecl ([],
                [], (* no storage classes *)
                BasicType Char,
                IdentifierDeclarator ([], anon_id ()),
                EmptyDecl, (* no asm *)
                None (* no initialiser *)
              ),
            None (* no bitfield *)
            )
          ])
        in
        normalise_type (SUEType (attrs, kind, tag, [single_char_decl]))
    *)

    | SUEType (attrs, kind, "", members) ->
        normalise_type {
          ty with
          t = SUEType (attrs, kind, anon_sue (), members)
        }

    | _ -> Visit.map_type normalise_struct ty

  and normalise_stmt = function
    | stmt -> Visit.map_stmt normalise_struct stmt

  and normalise_expr = function
    | expr -> Visit.map_expr normalise_struct expr


  in normalise_decl
