
let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []

let is_bracket = let rec contains lst value = match lst with
  | [] -> false
  | h::_ when h = value -> true
  | _::t -> contains t value
in contains (to_char_list "{}()[]")

type bracket_type_t = Curly | Square | Parenthesis

type bracket_side_t = Open | Close

let bracket_type = function
| x when x = '{' || x = '}' -> Curly
| x when x = '[' || x = ']' -> Square
| x when x = '(' || x = ')' -> Parenthesis
| _ -> failwith "Only brackets have bracket type"

let bracket_side = function
| x when x = '{' || x = '(' || x = '[' -> Open
| x when x = '}' || x = ')' || x = ']' -> Close
| _ -> failwith "Only brackets have bracket side"

let rec stripe acc lst = match lst with
| [] -> acc
| h::t when is_bracket h -> stripe (acc@[h]) t
| _::t -> stripe acc t

let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []


module Stack = struct
  let empty = []

  let is_empty s = s = []

  let push s value = [value]@s

  let peak = function
    | [] -> None
    | h::_ -> Some h

  let pop = function
    | [] -> []
    | _::t -> t

end

let are_balanced expression = expression |> to_char_list |> stripe [] |> let rec _traverse stack lst = match lst with
| [] -> Stack.is_empty stack
| h::t when bracket_side h = Open -> _traverse (Stack.push stack h) t
| h::t -> match Stack.peak stack with
    | None -> false
    | Some b -> match bracket_type b = bracket_type h with
      | true -> _traverse (Stack.pop stack) t
      | false -> false
in _traverse Stack.empty
