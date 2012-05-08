let of_char = String.make 1


let fold_left f a s =
  let rec fold n f a s =
    if n = String.length s then
      a
    else
      fold (n + 1) f (f a s.[n]) s
  in
  fold 0 f a s


let fold_right f s b =
  let rec fold n f s b =
    if n = 0 then
      b
    else
      fold (n - 1) f s (f s.[n - 1] b)
  in
  fold (String.length s) f s b


let rec nsplit s sep =
  assert (String.length sep = 1);
  try
    let pos = String.index s sep.[0] in
    String.sub s 0 pos :: nsplit (String.sub s (pos + 1) (String.length s - pos - 1)) sep
  with Not_found ->
    [s]


let of_foldable length fold_left l =
  let res = String.create (length l) in
  ignore (fold_left (fun i c ->
    res.[i] <- c;
    i + 1
  ) 0 l);
  res


let of_list = of_foldable List.length List.fold_left
let of_array = of_foldable Array.length Array.fold_left
