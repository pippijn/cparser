open Ast


let rec add_attrs attrs decl =
  match attrs with
  | [] -> decl
  | attrs ->
      { decl with
        d =
          match decl.d with
          | EmptyDecl ->
              IdentifierDeclarator (List.flatten attrs, "")
          | IdentifierDeclarator (orig, id) ->
              IdentifierDeclarator (List.flatten attrs @ orig, id)
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
