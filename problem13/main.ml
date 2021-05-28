(* # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; *)
module Rle =
    struct
        type 'a rle =
            | One of 'a
            | Many of int * 'a;;

        let make t =
            let (fst, snd) = t in
                if (fst > 1) then Many (fst ,snd) else One snd;;
    end;;

let encode l =
    let rec aux count acc = function
        | [] -> []
        | [x] -> (Rle.make(count + 1, x) :: acc)
        | fst :: (snd :: list as rest) -> if fst = snd then aux (count + 1) acc rest
                                          else aux 0 (Rle.make(count + 1, fst) :: acc) rest
    in
        List.rev (aux 0 [] l);;
