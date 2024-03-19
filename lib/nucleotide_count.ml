
let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []

let char_to_string = let open String in make 1

let nucleotides = to_char_list "ACGT"

let is_nucleotide = let rec _is lst c = match lst with
| [] -> false
| h::_ when h = c -> true
| _::t -> _is t c
in _is nucleotides

let not = function
| true -> false
| false -> true

exception F of char

let check s = let rec _check tail s = match tail with
| [] -> s
| h::_ when not (is_nucleotide h) -> raise (F h)
| _::t -> _check t s
in _check (to_char_list s) s


let _count_nucleotide s c = let rec _cnt acc lst = match lst with
| [] -> acc
| h::t when h = c -> _cnt (acc + 1) t
| _::t -> _cnt (acc) t
in _cnt 0 (to_char_list s)

let count_nucleotide s c =
  try Ok (
    _count_nucleotide (s |> check) (if not (is_nucleotide c) then raise (F c) else c)
  ) with F msg -> Error msg

let count_nucleotides s = let open Base in let rec _count acc s lst = match lst with
  | [] -> acc
  | h::t -> let cnt = _count_nucleotide s h in if cnt > 0 then _count (Map.set acc ~key:h ~data:cnt) s t else acc
in try Result.Ok (
  _count (Map.empty (module Char)) (check s) nucleotides
)
with F msg -> Result.Error msg
