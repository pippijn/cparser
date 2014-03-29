open Ast


let member_types sue =
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

  match sue with
  | SUEType (_, _, _, members) ->
      List.rev (List.fold_left collect [] members)
  | _ ->
      failwith "expected SUEType"


let member_decls sue =
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

  match sue.t with
  | SUEType (_, _, _, members) ->
      List.rev (List.fold_left collect [] members)
  | _ -> die (Type_error ("Sue.member_decls", None, [sue]))


let kind_of = function
  | { t = SUEType (_, kind, _, _) } -> kind
  | ty -> die (Type_error ("Sue.kind_of", None, [ty]))


let is_decl = function
  | { t = SUEType (_, _, _, []) } -> true
  | { t = SUEType (_, _, _,  _) } -> false
  | ty -> die (Type_error ("Sue.is_decl", None, [ty]))
