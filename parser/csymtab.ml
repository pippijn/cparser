type csym =
  | Decl of Ast.declaration
  | Type of Ast.ctype


let decl_of_csym (Decl decl) = decl
let type_of_csym (Type ctyp) = ctyp


type sym = csym Symtab.sym

type t = csym Symtab.t

let tab : t = Symtab.create ()

let enter_scope = Symtab.enter_scope tab
let leave_scope () = Symtab.leave_scope tab

let insert = Symtab.insert tab
let insert_decl name symtype decl = insert name symtype (Decl decl)
let insert_type name symtype ctyp = insert name symtype (Type ctyp)

let replace = Symtab.replace tab
let replace_decl name symtype decl = replace name symtype (Decl decl)
let replace_type name symtype ctyp = replace name symtype (Type ctyp)

let lookup = Symtab.lookup tab
let lookup_decl name symtype = decl_of_csym (lookup name symtype)
let lookup_type name symtype = type_of_csym (lookup name symtype)

let print () =
  Symtab.print (function
    | Decl decl -> Codegen.code_of_decl decl
    | Type ctyp -> Codegen.code_of_type ctyp
  ) tab