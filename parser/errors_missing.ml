type error_table = (int * C_tokens.token, string) Hashtbl.t

let missing : error_table = Hashtbl.create 10

let add state token code =
  let key = state, C_tokens.EOF in
  try
    let oldcode = Hashtbl.find missing key in
    if String.length oldcode > String.length code then
      Hashtbl.replace missing key code
  with Not_found ->
    Hashtbl.add missing key code


let get () =
  Hashtbl.fold (fun _ code msgs ->
    code :: msgs
  ) missing []
