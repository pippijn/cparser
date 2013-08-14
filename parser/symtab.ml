type symtype =
  | Label
  | Tag
  | Member
  | Ordinary

type 'a sym = {
  scope : string;
  name : string;
  namespace : symtype;
  decl : 'a;
}

exception Name_clash of string

type 'a scope = (string * symtype, 'a) Hashtbl.t

type 'a t = {
  tables : (string, 'a scope) Hashtbl.t;
  mutable stack : 'a scope list;
}


let string_of_symtype = function
  | Label -> "label"
  | Tag -> "tag"
  | Member -> "member"
  | Ordinary -> "ordinary"


let enter_scope symtab name =
  assert (name <> "");
  let scope = 
    try
      Hashtbl.find symtab.tables name
    with Not_found ->
      let scope = Hashtbl.create 10 in
      Hashtbl.add symtab.tables name scope;
      scope
  in
  symtab.stack <- scope :: symtab.stack


let leave_scope symtab =
  symtab.stack <-
    match symtab.stack with
    (* This really can't happen, as the second case catches an underflow
     * before we reach a completely empty list. *)
    | [] -> raise ExPervasives.Impossible
    | _::[] -> failwith "scope stack underflow"
    | _::tl -> tl


let lookup symtab name symtype =
  let rec find name = function
    | scope :: parents ->
        begin try
          Hashtbl.find scope (name, symtype)
        with Not_found ->
          find name parents
        end
    | [] -> raise Not_found
  in
  find name symtab.stack


let lookup_in_scope symtab scope name symtype =
  Hashtbl.find (Hashtbl.find symtab.tables scope) (name, symtype)


let insert symtab name symtype decl =
  assert (name <> "");
  let scope = List.hd symtab.stack in
  try
    ignore (lookup symtab name symtype);
    raise (Name_clash name)
  with Not_found ->
    Hashtbl.add scope (name, symtype) decl


let insert_into_scope symtab { scope = scope; name = name; namespace = symtype; decl = decl } =
  assert (name <> "");
  enter_scope symtab scope;
  insert symtab name symtype decl;
  leave_scope symtab


let replace symtab name symtype decl =
  assert (name <> "");
  let scope = List.hd symtab.stack in
  (* Try to look up the symbol to ensure it is there. *)
  ignore (lookup symtab name symtype);
  Hashtbl.replace scope (name, symtype) decl


let replace_in_scope symtab { scope = scope; name = name; namespace = symtype; decl = decl } =
  assert (name <> "");
  enter_scope symtab scope;
  replace symtab name symtype decl;
  leave_scope symtab


let create () =
  let symtab = {
    tables = Hashtbl.create 10;
    stack = [];
  } in
  enter_scope symtab "global";
  symtab


let reset symtab =
  Hashtbl.clear symtab.tables;
  symtab.stack <- [];
  enter_scope symtab "global"


let print string_of_decl symtab =
  assert (List.length symtab.stack = 1);
  let tables =
    Hashtbl.fold (fun scope_name scope tables ->
      let scope =
        Hashtbl.fold (fun sym decl scope ->
          (sym, decl) :: scope
        ) scope []
      in

      let scope =
        List.sort (fun ((a, _), _) ((b, _), _) ->
          String.compare a b
        ) scope
      in

      (scope_name, scope) :: tables
    ) symtab.tables []
  in

  let tables =
    List.sort (fun (a, _) (b, _) ->
      String.compare a b
    ) tables
  in

  List.iter (fun (scope_name, scope) ->
    if scope <> [] then (
      Printf.printf "scope %s:\n" scope_name;
      List.iter (fun ((decl_name, symtype), decl) ->
        Printf.printf " - %s(%s): %s\n"
          decl_name
          (string_of_symtype symtype)
          (string_of_decl decl)
      ) scope
    )
  ) tables
