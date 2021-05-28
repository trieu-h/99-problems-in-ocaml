let encode l =
    let rec aux acc lst = function
        | [] -> []
        | [x] -> List.rev ((acc + 1, x) :: lst)
        | fst :: (snd :: tail as rest) -> if fst = snd then aux (acc + 1) lst rest else aux 0 ((acc + 1, fst) :: lst) rest
    in aux 0 [] l;;
