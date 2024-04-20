
let to_char_list = let rec _to_char_list acc s = let open String in match s with
  | "" -> acc
  | _ -> _to_char_list (acc@[s.[0]]) (sub s 1 (length s - 1))
in _to_char_list []

let rec reverse acc lst = match lst with
| [] -> acc
| h::t -> reverse ([h]@acc) t

type char_type_t = Numerical | Space | Invalid

let char_type c = let code = int_of_char c in match code with
| x when x >= 48 && x <= 57 -> Numerical
| 32 -> Space
| _ -> Invalid

let int_value c = int_of_char c - 48

let is_even x = x = (x / 2) * 2

let luhn_double n = let d = n * 2 in match d with
| x when x < 10 -> x
| _ -> d - 9

let rec luhn_sum acc proccessed_digits lst = match lst with
  | [] -> (match proccessed_digits with
    | x when x <= 1 -> 1
    | _ -> acc
  )
  | h::t -> (match char_type h with
    | Invalid -> 1
    | Space -> luhn_sum acc proccessed_digits t
    | Numerical -> let multiplied = match is_even proccessed_digits with
      | true -> int_value h
      | false -> h |> int_value |> luhn_double
    in luhn_sum (acc + multiplied) (proccessed_digits + 1) t
  )

let valid digits = let ls = digits |> to_char_list |> reverse [] |> luhn_sum 0 0 in ls = 10 * (ls / 10)
