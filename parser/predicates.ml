open Ast


let is_decl = function
  | WildcardDecl _
  | PreprocessorDirective _
  | DeclaringList _
  | ToplevelAsm _
  | FunctionDefinition _
  | TypedDecl _ -> true
  | _ -> false
