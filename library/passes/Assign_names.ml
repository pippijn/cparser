open Ast


let assign_names tu =
  let count = ref 0 in

  let update trs nm =
    let trs = Attributes.set_scope (nm ^ "#" ^ string_of_int !count) trs in
    count := !count + 1;
    trs
  in


  let rec assign_names_struct nm = Visit.({
    map_type = assign_names_type nm;
    map_expr = assign_names_expr nm;
    map_stmt = assign_names_stmt nm;
    map_decl = assign_names_decl nm;
  })


  and assign_names_decl nm =
    let resume nm = Visit.map_decl (assign_names_struct nm) in
    function
    (* XXX: SUE member scopes are here, because types currently have no traits *)
    | TypedDecl (trs, sc, (SUEType (_, _, nm, _) as ty), untyped, asm, init) ->
        let trs = update trs nm in
        resume nm (TypedDecl (trs, sc, ty, untyped, asm, init))
    (* Function declarations also have their own scope *)
    | TypedDecl (trs, sc, (FunctionType _ as ty), untyped, asm, init) ->
        let trs = update trs nm in
        resume nm (TypedDecl (trs, sc, ty, untyped, asm, init))
    | FunctionDefinition (dtrs, TypedDecl (trs, sc, ty, untyped, asm, init), body) ->
        let nm = Decls.decl_name untyped in
        let trs = update trs nm in
        let decl = resume nm (TypedDecl (trs, sc, ty, untyped, asm, init)) in
        FunctionDefinition (dtrs, decl, assign_names_stmt nm body)
    | n -> resume nm n


  and assign_names_expr nm = function
    | n -> Visit.map_expr (assign_names_struct nm) n


  and assign_names_stmt nm =
    let resume nm = Visit.map_stmt (assign_names_struct nm) in

    function
    | CompoundStatement (trs, stmts) ->
        let trs = update trs nm in
        CompoundStatement (trs, List.map (assign_names_stmt nm) stmts)
    | n -> resume nm n


  and assign_names_type nm = function
    | n -> Visit.map_type (assign_names_struct nm) n



  in

  assign_names_decl "global" tu
