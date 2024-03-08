
type nucleotide = A | C | G | T

let rec _len acc lst = match lst with
  | [] -> acc
  | _::t -> _len (acc + 1) t

let len = _len 0

let get_head lst = match lst with
  | [] -> failwith "can't get head of  an empty list"
  | h::t -> (h, t)

let rec sub_hamming_distance acc first second = match first with
  | [] -> acc
  | _ -> let
      (first_h, first_t) = get_head first
      and (second_h, second_t) = get_head second
      in sub_hamming_distance (acc + (if first_h = second_h then 0 else 1)) first_t second_t

let hamming_distance first second = let open Base.Result in
   match len first = len second with
  | true -> Ok (sub_hamming_distance 0 (A::first) (A::second))
  | false -> match first, second with
        | _, [] -> Error ("right strand must not be empty")
        | [], _ -> Error ("left strand must not be empty")
        | _, _ -> Error ("left and right strands must be of equal length")
