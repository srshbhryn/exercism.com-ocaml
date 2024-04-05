
let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []

let are_equivalent c0 c1 = match (int_of_char c0) - (int_of_char c1) with
| 0 -> true
| 32 -> true
| -32 -> true
| _ -> false


exception NotFound
let find_and_remove = let rec _far acc c lst = match lst with
| [] -> raise NotFound
| h::t when are_equivalent h c -> acc@t
| h::t -> _far (acc@[h]) c t
in _far []


let rec are_same_words w0 w1 = match w0, w1 with
| [], [] -> true
| [], _::_ -> false
| _::_, [] -> false
| h::t, x::y when are_equivalent h x -> are_same_words t y
| _ -> false

let are_anagrams word0 word1 = if are_same_words word0 word1 then false else let rec _aa word0 word1 = match word0, word1 with
  | [], [] -> true
  | _::_, [] -> false
  | [], _::_ -> false
  | h::t, _::_ -> _aa t (find_and_remove h word1)
in try (
  _aa word0 word1
) with NotFound -> false


let anagrams word = let rec _a word acc candids = match candids with
| [] -> acc
| h::t when are_anagrams word (to_char_list h) -> _a word (acc@[h]) t
| _::t -> _a word acc t
in _a (to_char_list word) []
