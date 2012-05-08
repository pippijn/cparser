let reduce f lst =
  List.fold_left f (List.hd lst) (List.tl lst)

let max lst =
  reduce max lst

let sum lst =
  reduce (+) lst
