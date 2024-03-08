
type nucleotide = A | C | G | T

let rec _len acc lst = match lst with
  | [] -> 0
  | _::h -> _len (acc + 1) h

let len lst = _len 0 lst

let get_head lst = match lst with
  | [] -> failwith "can't get head of  an empty list"
  | h::t -> (h, t)

let rec sub_hamming_distance acc first second = match first with
  | [] -> acc
  | _ -> let
      (first_h, first_t) = get_head first
      and (second_h, second_t) = get_head second
      in sub_hamming_distance (acc + (if first_h = second_h then 0 else 1)) first_t second_t

let hamming_distance first second = let first_l = (len first) and second_l = (len second) in let open Base.Result in
  match (first_l = second_l), (first_l > 0), (second_l > 0) with
  | true, false, false -> Ok (0)
  | _, false, true -> Error ("left strand must not be empty")
  | _, true, false -> Error ("right strand must not be empty")
  | true, true, true -> Ok (sub_hamming_distance 0 first second)
  | false, _,  _ -> Error ("left and right strands must be of equal length")
