
let verse n = match n with
| x when x > 100 || x < 0 -> failwith "invalid verse number"
| 0 -> "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall."
| 1 -> "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall."
| 2 -> "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall."
| _ -> let s = string_of_int n and ss = string_of_int (n - 1) in s ^ " bottles of beer on the wall, " ^ s ^ " bottles of beer.\nTake one down and pass it around, " ^ ss ^ " bottles of beer on the wall."


let recite = let rec _recite acc from until = let next_verse = match from with 
  | 0 -> 99
  |_ -> from - 1
  in match until with
    | 0 -> acc
    | _ -> let acc_and_sep = match acc with
      | "" -> ""
      | _ -> acc ^ "\n\n"
    in _recite (acc_and_sep ^ verse from) next_verse (until - 1 )
in _recite ""
