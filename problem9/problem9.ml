let pack l =
    let rec aux lst acc = function
        | [] -> []
        | [x] -> (x :: lst) :: acc
        | fst :: (snd :: tail as rest) -> if fst = snd then aux (fst :: lst) acc rest
                                          else aux [] ([fst :: lst] @ acc) rest in
    List.rev (aux [] [] l);;
