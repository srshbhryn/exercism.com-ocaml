
let to_list = let rec _to_list acc s = match s with
  | "" -> acc
  | _ -> let open String in _to_list (acc@[s.[0]]) ((sub s 1 (length s - 1)))
in _to_list []


type char_type = LowerCase | UpperCase | Punc | Seprator

let get_char_type c = let m = int_of_char c in match m with
| n when n > 96 && n < 123 -> LowerCase
| n when n > 64 && n < 91 -> UpperCase
| n when   n = 33 || n = 44 || n = 58 || n = 59 || n = 63 || n = 39 -> Punc
| n when n = 32 || n = 45 || n = 95 -> Seprator
| _ -> failwith ("unexpected char: " ^ let open String in make 1 c)


let capitalize c = match get_char_type c with
| UpperCase -> c
| LowerCase -> char_of_int (int_of_char c - 32)
| _ -> failwith "unexpected char to capitalize"

let to_string = let rec _to_string acc lst =  match lst with
  | [] -> acc
  | h::t -> _to_string (acc ^ (let open String in make 1 h)  ) t
in _to_string ""

let acronym s = let rec _acronym acc prev_letter lst = match lst, prev_letter with
| [], _ -> acc
| h::t, x when get_char_type h = Punc -> _acronym acc x t
| h::t, Seprator when get_char_type h = Seprator -> _acronym acc Seprator t
| h::t, Seprator when let ct = get_char_type h in ct = UpperCase || ct = LowerCase -> _acronym (acc@[capitalize h]) (get_char_type h) t
| _::t, Seprator -> _acronym acc Seprator t
| h::t, _ -> _acronym acc (get_char_type h) t

in s |> to_list |> _acronym [] Seprator |> to_string
