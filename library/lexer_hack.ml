type t = {
  mutable scopes : (string, bool) Hashtbl.t list;
}

let tab = {
  scopes = [];
}

let push_scope () = tab.scopes <- Hashtbl.create 0 :: tab.scopes
let pop_scope  () = tab.scopes <- List.tl tab.scopes
let depth      () = List.length tab.scopes

let typedef id =
  Hashtbl.add (List.hd tab.scopes) id true

let identifier id =
  Hashtbl.add (List.hd tab.scopes) id false

let is_typedef id =
  try
    Hashtbl.find (List.find (fun tab -> Hashtbl.mem tab id) tab.scopes) id
  with
  | Not_found -> false
