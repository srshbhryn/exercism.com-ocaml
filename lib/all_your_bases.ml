type base = int

let reverse = let rec _reverse acc lst = match lst with
| [] -> acc
| h::t -> _reverse ([h]@acc) t
in _reverse []

exception InvalidDigit

let whole_number digits base = let rec _whole base power acc digits = match  digits with
| [] -> acc
| h::t when h >= 0 && h < base -> _whole base (power * base) (acc + power * h) t
| _ -> raise InvalidDigit
in _whole base 1 0 digits

let biggest_power base number = let rec _biggest_power base number pow = if number < base then 1 else match number > pow with
| true -> _biggest_power base number (pow * base)
| false -> pow / base
in _biggest_power base number 1

let to_digit number base = if number = 0 then [0] else
    let bp = biggest_power base number in
        let rec _to_digits acc pow number base =
            let new_digit = number / pow in
                match pow with
                    | 1 -> acc@[new_digit]
                    | _ -> _to_digits (acc@[new_digit]) (pow / base) (number - pow * new_digit) base
in _to_digits [] bp number base


let convert_bases ~from ~digits ~target = match from <= 1 || target <= 1 with
    | true -> None
    | false -> try
            let number = whole_number (reverse digits) from in Some (to_digit number target)
        with InvalidDigit -> None
