
type char_type = LowerCase | UpperCase | Numerical | Other

let get_char_type c = let m = int_of_char c in match m with
| n when n > 96 && n < 123 -> LowerCase
| n when n > 46 && n < 58 -> Numerical
| n when n > 64 && n < 91 -> UpperCase
| n when n > 64 && n < 91 -> UpperCase
| _ -> Other

let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []

let rec remove_white_spaces acc lst= match lst with
| [] -> acc
| ' '::t -> remove_white_spaces acc t
| '\r'::t -> remove_white_spaces acc t
| '\n'::t -> remove_white_spaces acc t
| '\t'::t -> remove_white_spaces acc t
| h::t -> remove_white_spaces (acc@[h]) t

let rec is_a_question sentence = match sentence with
  | [] -> false
  | '?'::[] -> true
  | _::t -> is_a_question t

let rec is_silence = function
| [] -> true
| ' '::t -> is_silence t
| _::_ -> false

let rec is_yelling acc = function
| h::_ when get_char_type h = LowerCase -> false
| h::t -> is_yelling (acc || get_char_type h = UpperCase) t
| [] -> acc

let response_for sentence = let sentence_char_list =   sentence |> to_char_list |> remove_white_spaces [] in
  match is_silence sentence_char_list, is_a_question sentence_char_list, is_yelling false sentence_char_list with
    | true, _, _ -> "Fine. Be that way!"
    | _, true, false -> "Sure."
    | _, true, true -> "Calm down, I know what I'm doing!"
    | _, false, true -> "Whoa, chill out!"
    | _, false, false -> "Whatever."
