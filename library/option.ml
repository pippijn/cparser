exception No_value
let no_value = No_value


let may f = function
  | Some x -> f x
  | None -> ()


let map f = function
  | Some x -> Some (f x)
  | None -> None


let map_default f x = function
  | Some x -> f x
  | None -> x


let fold f a = function
  | Some x -> f a x
  | None -> a


let is_none = function
  | Some _ -> false
  | None -> true


let is_some opt = not (is_none opt)


let get = function
  | Some x -> x
  | None -> raise no_value


let default x = function
  | Some x -> x
  | None -> x
