
let rec len acc = function
  | [] -> acc
  | _::t -> len (acc + 1) t

let rec break_to_halves first_half second_half first_half_size second_half_size = match first_half_size >= second_half_size - 1, second_half with
  | true, _ -> first_half, second_half, first_half_size, second_half_size
  | false, h::t -> break_to_halves (first_half@[h]) t (first_half_size + 1) (second_half_size - 1)
  | false, [] -> [], [], 0, 0

let rec _find offset lst value =
    let first_hlaf, second_half, first_half_size, _ = break_to_halves [] lst 0 (len 0 lst)
    in match second_half with
      | [] -> let open Base.Result in Error("value not in array")
      | h::_ when h > value -> (
        match first_half_size with
          | 0 -> let open Base.Result in Error("value not in array")
          | _ ->  _find offset first_hlaf value
      )
      | h::[] when h < value -> let open Base.Result in Error("value not in array")
      | h::_ when h < value -> _find (offset + first_half_size) second_half value
      | _::_ -> let open Base.Result in Ok(offset + first_half_size)

let find arr = let open Array in _find 0 (to_list arr)
