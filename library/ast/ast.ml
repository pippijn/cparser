open Sexplib.Conv

#include "ast.mli"

let die error =
  raise (ASTError error)
