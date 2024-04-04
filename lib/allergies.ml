
type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let allergen_to_index = function
  | Eggs -> 0
  | Peanuts -> 1
  | Shellfish -> 2
  | Strawberries -> 3
  | Tomatoes -> 4
  | Chocolate -> 5
  | Pollen -> 6
  | Cats -> 7

let biggest_two_power = let rec _btp p n = if p > n then p / 2 else _btp (p * 2) n
in _btp 1

let digits n = if n = 0 then [] else
    let rec _digits acc current_power residual = match residual >= current_power, current_power with
    | true, 1 -> [1]@acc
    | false, 1 -> [0]@acc
    | true, _ -> _digits ([1]@acc) (current_power / 2) (residual - current_power)
    | false, _ -> _digits ([0]@acc) (current_power / 2) residual
in _digits [] (biggest_two_power n) n

let allergic_to code a = let rec _sub code_digits ai = match code_digits, ai with
| [], _ -> false
| h::_, 0 when h = 1 -> true
| _::t, _ -> _sub t  (ai - 1)
and ai = allergen_to_index a and code_digits = digits code in _sub code_digits ai

let allergens = [
  Eggs;
  Peanuts;
  Shellfish;
  Strawberries;
  Tomatoes;
  Chocolate;
  Pollen;
  Cats;
]

let allergies code = let rec _allergies acc allergen_lst code_lst =  match allergen_lst, code_lst with
| [], _ -> acc
| _, [] -> acc
| h::t, hh::tt -> _allergies (if hh = 1 then acc@[h] else acc) t tt
in _allergies [] allergens (digits code)
