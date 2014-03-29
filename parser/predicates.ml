open Ast


let is_decl decl =
  match decl.d with
  | WildcardDecl _
  | PreprocessorDirective _
  | DeclaringList _
  | ToplevelAsm _
  | FunctionDefinition _
  | TypedDecl _ -> true
  | _ -> false
