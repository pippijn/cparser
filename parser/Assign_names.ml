open Ast


let assign_names tu =
  let count = ref 0 in

  let next_name nm =
    let name = nm ^ "#" ^ string_of_int !count in
    count := !count + 1;
    name
  in

  let rec assign_names_struct nm = Visit.({
    map_type = assign_names_type nm;
    map_expr = assign_names_expr nm;
    map_stmt = assign_names_stmt nm;
    map_decl = assign_names_decl nm;
  })


  and assign_names_decl nm decl =
    let resume nm = Visit.map_decl (assign_names_struct nm) in
    match decl.d with
    (* XXX: SUE member scopes are here, because types currently have no traits *)
    | TypedDecl (_, sc, (SUEType (_, _, nm, _) as ty), untyped, asm, init) ->
        let scope = next_name nm in
        resume nm {
          decl with
          d = TypedDecl (scope, sc, ty, untyped, asm, init)
        }
    (* Function declarations also have their own scope *)
    | TypedDecl (_, sc, (FunctionType _ as ty), untyped, asm, init) ->
        let scope = next_name nm in
        resume nm {
          decl with
          d = TypedDecl (scope, sc, ty, untyped, asm, init)
        }
    | FunctionDefinition ({ d = TypedDecl (_, sc, ty, untyped, asm, init) },
                          body) ->
        let nm = Decls.decl_name untyped in
        let scope = next_name nm in
        let decl =
          resume nm {
            decl with
            d = TypedDecl (scope, sc, ty, untyped, asm, init)
          }
        in
        { decl with
          d = FunctionDefinition (decl, assign_names_stmt nm body)
        }
    | _ -> resume nm decl


  and assign_names_expr nm = function
    | n -> Visit.map_expr (assign_names_struct nm) n


  and assign_names_stmt nm stmt =
    let resume nm = Visit.map_stmt (assign_names_struct nm) in

    match stmt.s with
    | CompoundStatement (scope, stmts) ->
        let scope = next_name nm in
        { stmt with
          s = CompoundStatement (scope, List.map (assign_names_stmt nm) stmts) }
    | _ -> resume nm stmt


  and assign_names_type nm = function
    | n -> Visit.map_type (assign_names_struct nm) n



  in

  assign_names_decl "global" tu
