
module Set = struct
  type set = Empty | Node of set * char * set

  let rec add set value = match set with
  | Empty -> Node(Empty, value, Empty)
  | Node(l, v, r) -> match v = value, v > value with
    | true, _ -> set
    | false, false -> Node(add l value, v, r)
    | false, true -> Node(l, v, add r value)

  let rec size set = match set with
  | Empty -> 0
  | Node(l, _, r) -> 1 + size l + size r

end

let to_list = let rec _to_list acc s = match s with
  | "" -> acc
  | _ -> let open String in _to_list (acc@[s.[0]]) ((sub s 1 (length s - 1)))
in _to_list []

type char_type = LowerCase | UpperCase | Other

let get_char_type c = let m = int_of_char c in match m with
| n when n > 96 && n < 123 -> LowerCase
| n when n > 64 && n < 91 -> UpperCase
| _ -> Other


let capitalize c = match get_char_type c with
| UpperCase -> c
| LowerCase -> char_of_int (int_of_char c - 32)
| _ -> failwith "unexpected char to capitalize"


let rec to_alph_set acc lst = match lst with
| [] -> acc
| h::t -> match get_char_type h with
  | LowerCase -> let c = capitalize h in to_alph_set (Set.add acc c) t
  | UpperCase -> to_alph_set (Set.add acc h) t
  | Other -> to_alph_set acc t

let is_pangram str = str |> to_list |> (to_alph_set Set.Empty) |> Set.size |> let f x = x = 26 in f
