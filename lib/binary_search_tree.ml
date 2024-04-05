open Base

type bst = EMPTY | NODE of int * bst * bst

let empty = EMPTY

let value = function
  | EMPTY -> Base.Result.Error "Empty tree has no value"
  | NODE(x, _ , _) -> Base.Result.Ok x

let left tree = match tree with
| EMPTY -> Base.Result.Error "Empty tree has left subtree"
| NODE(_, x, _) -> Base.Result.Ok x

let right tree = match tree with
| EMPTY -> Base.Result.Error "Empty tree has right subtree"
| NODE(_, _, x) -> Base.Result.Ok x

let rec insert value tree = match tree with
| EMPTY -> NODE(value, EMPTY, EMPTY)
| NODE(x, l, r) when x >= value -> NODE(x, insert value l, r)
| NODE(x, l, r) when x < value -> NODE(x, l, insert value r)
| _ -> failwith "unexcpeted"

let rec to_list tree = match tree with
| EMPTY -> []
| NODE(v, l, r) -> (to_list l)@[v]@(to_list r)

