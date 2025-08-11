

let rec has_divider n lst = match lst with
  | [] -> false
  | h::t -> if n mod h = 0 then true else has_divider n t
;;
let range = let rec _range acc a b = match a <= b with
  | false -> acc
  | true -> _range (b::acc) a (b - 1)
in _range []
;;

let primes n = let lst = range 2 n in let rec _sieve acc lst = match lst with
  | [] -> acc
  | h::t -> let new_acc = if has_divider h acc then acc else h::acc in _sieve new_acc t
in _sieve [] lst |> List.rev