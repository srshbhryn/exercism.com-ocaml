

let rec contains lst c = match lst with
| [] -> false
| h::_ when h = c-> true
| _::t -> contains t c

let is_punctuation = contains [
  '!'; '"'; '#'; '$'; '%'; '&';
   '('; ')'; '*'; '+'; ','; '-'; '.'; '/';
  ':'; ';'; '<'; '='; '>'; '?'; '@';
  '['; '\\'; ']'; '^'; '_'; '`';
  '{'; '|'; '}'; '~'
]

let is_whitespace = contains [
  ' ';  (* space *)
  ',';  (* comma *)
  '\t'; (* tab *)
  '\n'; (* line feed *)
  '\r'; (* carriage return *)
  '\x0b'; (* vertical tab *)
  '\x0c'  (* form feed *)
]


let remove_wrapping_apostrophes s = let len = String.length s in
  if len >= 2 && s.[0] = '\'' && s.[len - 1] = '\''
  then String.sub s 1 (len - 2)
  else s

let char_list_to_word_list
    = let rec _cltwl word_list current_word_chars char_list
        = let new_world_list char_list words = match char_list  with
        | [] -> word_list
        | _ -> (char_list |> List.rev |> List.to_seq |> String.of_seq |> String.lowercase_ascii |> remove_wrapping_apostrophes) :: words
        in match char_list with
        | h::t when is_whitespace h -> _cltwl (new_world_list current_word_chars word_list) [] t
        | h::t when is_punctuation h -> _cltwl word_list current_word_chars t
        | h::t -> _cltwl word_list (h::current_word_chars) t
        | [] -> new_world_list current_word_chars word_list
in _cltwl [] [] 


let string_to_char_list s =
  List.init (String.length s) (String.get s)

open Base
let empty = Map.empty (module String)
let touch t s =
let count =
match Map.find t s with
| None -> 0
| Some x -> x
in
Map.set t ~key:s ~data:(count + 1)


let word_count text = let words = text |> string_to_char_list |> char_list_to_word_list
in List.fold words ~init:empty ~f:touch