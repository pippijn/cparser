open Sexplib.Conv
open Lexing

type 'a attribute = (string * 'a list) list

and 'a attributes_position_scope = [`Attribute of 'a attribute | `Position of Location.t | `Scope of string]
and 'a attributes_position = [`Attribute of 'a attribute | `Position of Location.t]
and 'a attributes_scope = [`Attribute of 'a attribute | `Scope of string]
and 'a attributes = [`Attribute of 'a attribute]
and position_scope = [`Position of Location.t | `Scope of string]
and position = [`Position of Location.t]
and scope = [`Scope of string]
with sexp


let attribute_opt traits =
  List.fold_left (fun found -> function
    | `Attribute attrs -> Some attrs
    | _ -> found
  ) None traits

let attribute traits = Option.get (attribute_opt traits)


let position_opt traits =
  List.fold_left (fun found -> function
    | `Position pos -> Some pos
    | _ -> found
  ) None traits

let position traits = Option.get (position_opt traits)


let scope_opt traits =
  List.fold_left (fun found -> function
    | `Scope scope -> Some scope
    | _ -> found
  ) None traits

let scope traits = Option.get (scope_opt traits)


let set extract inject default traits =
  List.sort compare (
    let modified, traits =
      List.fold_left (fun (modified, traits) trait ->
        match extract trait with
        | Some x -> true, inject x :: traits
        | None -> modified, trait :: traits
      ) (false, []) traits
    in
    if not modified then
      inject default :: traits
    else
      traits
  )


let add_attribute attr =
  set
    (function `Attribute attrs -> Some attrs | _ -> None)
    (fun a -> `Attribute (attr @ a))
    []

let set_position pos =
  set
    (function `Position _ -> Some () | _ -> None)
    (fun _ -> `Position pos)
    ()

let set_scope scope =
  set
    (function `Scope _ -> Some () | _ -> None)
    (fun _ -> `Scope scope)
    ()
