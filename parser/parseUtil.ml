open Ast
open Tqual
open Sclass

let ast_from_opt opt =
  match opt with
  | Some e -> e
  | None -> Decls.empty

let list_from_opt opt =
  match opt with
  | Some e -> e
  | None -> []

let list_pair_from_opt opt =
  match opt with
  | Some (a, b) -> a, b
  | None -> [], []

let singleton_list_from_opt opt =
  match opt with
  | Some e -> [e]
  | None -> []

let string_of_attrib (name, args) =
  name ^
    match args with
    | hd::tl -> "(" ^ List.fold_left (fun x a -> x ^ ", " ^ a) hd tl ^ ")"
    | _ -> ""


let merge_string_literals literal info =
  let rec collect strs strkind = function
    | (kind, lit) :: tl when strkind <> LIT_WString ->
        collect (lit :: strs) kind tl
    | (_, lit) :: tl ->
        collect (lit :: strs) strkind tl
    | [] -> strkind, (List.rev strs)
  in
  let kind, strs = collect [] LIT_String info in
  StringLiteral (kind, strs)


let default_int = PartialBasicType [BT_Default]

let abstract_decl ty =
  { d = TypedDecl ("", Sclass.empty, ty, Decls.empty, Decls.empty, None);
    d_sloc = Location.dummy; }

let attr = Traits.add_attrs

let sue_attr attrs loc =
  Attributes.add_attribute (List.flatten attrs) loc

let identity a = a
