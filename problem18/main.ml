let slice list i k =
    let rec aux curr acc = function
        | [] -> List.rev acc
        | x :: xs -> let rest = if (i <= curr && curr <= k) then x :: acc else acc
                     in aux (curr + 1) rest xs
    in aux 0 [] list

