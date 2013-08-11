#define datatype __fc_datatype

datatype BinaryTree =
  | Leaf (data : void *)
  | Node (left : BinaryTree, right : BinaryTree)
  ;
