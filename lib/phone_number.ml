let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []
let range = let rec _range acc x y = if x = y then
  acc else _range (acc@[x]) (x+1) y
in _range []
let head = function
| [] -> failwith "empty list has no head"
| h::_ -> h
let tail = function
| [] -> failwith "empty list has no tail"
| _::t -> t
let ndigits = ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let xdigits = ['0'; '1']@ndigits
let rec does_contain is_a_candidate = function
  | [] -> false
  | h::t -> if is_a_candidate h then true else does_contain is_a_candidate t
let is_xdigit n = let eq x = (x = n) in does_contain eq xdigits
let is_ndigit n = let eq x = (x = n) in does_contain eq ndigits
let clean = let rec _clean acc lst = match lst with
  | [] -> acc
  | h::t -> _clean (if is_xdigit h then acc@[h] else acc) t
in _clean []
let char_to_string c = let open String in make 1 c
let to_string = let rec _to_string acc lst =  match lst with
  | [] -> acc
  | h::t -> _to_string (acc ^ char_to_string h  ) t
in _to_string ""
let len = let rec _len acc lst = match lst with
  | [] -> acc
  | _::t -> _len (acc + 1) t
in _len 0
let is_a_letter c = let n = int_of_char c in (
  (n > 96 && n < 123) || (n > 64 && n < 91)
)
let is_a_punctuation c = let n = int_of_char c in (
  n = 33 || n = 44 || n = 58 || n = 59 || n = 63
)
let check_letters lst = if does_contain is_a_letter lst then failwith "letters not permitted" else lst
let check_punctuations lst = if does_contain is_a_punctuation lst then failwith "punctuations not permitted" else lst
let check_north_america_code_and_remove_it lst = match head lst, len lst with
  | '1', 11 -> tail lst
  | _, 10 -> lst
  | _, 11 -> failwith "11 digits must start with 1"
  | _, x when x > 11 -> failwith "more than 11 digits"
  | _, _ -> failwith "incorrect number of digits"
let rec n_th_head acc lst = if acc = 0 then head lst else n_th_head (acc - 1) (tail lst)
let check_area_code lst = match n_th_head 0 lst with
| '0' -> failwith "area code cannot start with zero"
| '1' -> failwith "area code cannot start with one"
| _ -> lst
let check_exchange_code lst =match n_th_head 3 lst with
| '0' -> failwith "exchange code cannot start with zero"
| '1' -> failwith "exchange code cannot start with one"
| _ -> lst
let number n = let open Base.Result in
  try Ok (
    n |> to_char_list |> check_letters |> check_punctuations |> clean |> check_north_america_code_and_remove_it |> check_area_code |> check_exchange_code |> to_string
  )
  with Failure msg -> Error msg
