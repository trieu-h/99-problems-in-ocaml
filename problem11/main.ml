module Rle =
    struct
        type 'a rle =
            | One of 'a
            | Many of int * 'a;;

        let make t =
            let (fst, snd) = t in
                if (fst > 1) then Many (fst ,snd) else One snd;;
    end;;

(* Reusing solution in problem 10 *)
let encode l =
    let rec aux acc lst = function
        | [] -> []
        | [x] -> List.rev ((acc + 1, x) :: lst)
        | fst :: (snd :: tail as rest) -> if fst = snd then aux (acc + 1) lst rest else aux 0 ((acc + 1, fst) :: lst) rest
    in aux 0 [] l;;
(* Reusing solution in problem 10 *)

let to_tle l = l |> encode |> List.map Rle.make;;

