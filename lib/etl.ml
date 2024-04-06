
type element = char * int

type bst = EMPTY | NODE of element * bst * bst

let get_order e = let c, _ = e in int_of_char c

let empty = EMPTY

let rec insert value tree = match tree with
| EMPTY -> NODE(value, EMPTY, EMPTY)
| NODE(x, l, r) when get_order x >= get_order value -> NODE(x, insert value l, r)
| NODE(x, l, r) when get_order x < get_order value  -> NODE(x, l, insert value r)
| _ -> failwith "unexcpeted"

let rec to_list tree = match tree with
| EMPTY -> []
| NODE(v, l, r) -> (to_list l)@[v]@(to_list r)

let lower_case c = char_of_int (int_of_char c + 32)

let rec insert_chars_to_tree score char_list tree = match char_list with
| [] -> tree
| h::t -> insert_chars_to_tree score t (insert (lower_case h, score) tree)

let transform data =  let rec _t tree data = match data with
| [] -> tree
| h::t -> let s, lst = h in _t (insert_chars_to_tree  s  lst tree) t
in to_list (_t empty data)
