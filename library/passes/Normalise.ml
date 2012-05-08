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

  let anon_decl () = IdentifierDeclarator ([], anon_id ()) in


  let rec normalise_struct = Visit.({
    map_type = normalise_type;
    map_decl = normalise_decl;
    map_stmt = normalise_stmt;
    map_expr = normalise_expr;
  })


  and normalise_decl = function
    | IdentifierDeclarator (trs, "") ->
        IdentifierDeclarator (trs, anon_id ())
    | TypedDecl (trs, sc, (SUEType _ as ty), Nothing, asm, init) when Sclass.is_typedef sc ->
        normalise_decl (TypedDecl (trs, sc, ty, anon_decl (), asm, init))
    | StructDeclarator (trs, TypedDecl (dtrs, sc, ty, Nothing, asm, init), bitfield) ->
        normalise_decl (StructDeclarator (trs, TypedDecl (dtrs, sc, ty, anon_decl (), asm, init), bitfield))

    | n -> Visit.map_decl normalise_struct n


  and normalise_type = function
    | PartialBasicType [BT_Default] ->
        BasicType SInt

    | PartialBasicType bts ->
        BasicType (Basic_type.of_list bts)

    (* XXX: not good
    | SUEType (attrs, kind, tag, [Nothing]) ->
        let single_char_decl =
          DeclaringList ([], [
            StructDeclarator ([],
              TypedDecl ([],
                [], (* no storage classes *)
                BasicType Char,
                IdentifierDeclarator ([], anon_id ()),
                Nothing, (* no asm *)
                None (* no initialiser *)
              ),
            None (* no bitfield *)
            )
          ])
        in
        normalise_type (SUEType (attrs, kind, tag, [single_char_decl]))
    *)

    | SUEType (attrs, kind, "", members) ->
        normalise_type (SUEType (attrs, kind, anon_sue (), members))

    | ty -> Visit.map_type normalise_struct ty

  and normalise_stmt = function
    | stmt -> Visit.map_stmt normalise_struct stmt

  and normalise_expr = function
    | expr -> Visit.map_expr normalise_struct expr


  in normalise_decl
