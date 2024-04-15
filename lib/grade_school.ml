open Base

module Int_map = Map.M(Int)

module Bst = struct
  type tree = Empty | Node of tree * string * tree
  let empty = Empty

  let rec add tree value = match tree with
  | Empty -> Node(Empty, value, Empty)
  | Node(left, v, right) -> if compare_string value v  <= 0 then Node(add left value, v, right) else Node(left, v, add right value)

  let rec to_list t = match t with
  | Empty -> []
  | Node(left, v, right) ->  (to_list left)@[v]@(to_list right)


end

type school = Bst.tree Int_map.t

let empty_school = Map.empty (module Int)

let add student grade school = let students = match Map.find school grade with
| None -> Bst.add Bst.empty student
| Some t -> Bst.add t student
in Map.set school ~key:grade ~data:students

let grade grade school = match Map.find school grade with
| None -> []
| Some lst -> Bst.to_list lst

let sorted _ =
    failwith "'sorted' is missing"


let roster school = let rec _roster grades acc school = match grades with
  | [] -> acc
  | h::t -> let studests = match Map.find school h with
    | None -> []
    | Some tree -> Bst.to_list tree
  in _roster t (acc@studests) school
in _roster (Map.keys school) [] school
