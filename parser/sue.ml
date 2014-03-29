open Ast


let member_types =
  let rec collect types decl =
    match decl.d with
    | DeclaringList (decls) ->
        List.fold_left collect types decls
    | StructDeclarator (decl, _) ->
        collect types decl
    | TypedDecl (_, _, ty, _, _, _) ->
        ty :: types
    | _ -> die (Declaration_error ("Sue.member_types", None, [decl]))
  in

  function
  | SUEType (_, _, _, members) ->
      List.rev (List.fold_left collect [] members)
  | ty -> die (Type_error ("Sue.member_types", None, [ty]))


let member_decls =
  let rec collect fields decl =
    match decl.d with
    | DeclaringList (decls) ->
        List.fold_left collect fields decls
    | StructDeclarator (decl, _) ->
        collect fields decl
    | TypedDecl _ ->
        decl :: fields
    | _ -> die (Declaration_error ("Sue.member_decls", None, [decl]))
  in

  function
  | SUEType (_, _, _, members) ->
      List.rev (List.fold_left collect [] members)
  | ty -> die (Type_error ("Sue.member_decls", None, [ty]))


let kind_of = function
  | SUEType (_, kind, _, _) -> kind
  | ty -> die (Type_error ("Sue.kind_of", None, [ty]))


let is_decl = function
  | SUEType (_, _, _, []) -> true
  | SUEType (_, _, _, _) -> false
  | ty -> die (Type_error ("Sue.is_decl", None, [ty]))
