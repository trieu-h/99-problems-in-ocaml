let split lst delimiter =
    let rec aux count (fst, snd) = function
        | [] -> ([], [])
        | list when count > List.length list -> (list, [])
        | x :: xs when count = 0 -> (fst |> List.rev, xs)
        | x :: xs when count > 0 -> aux (count - 1) (x :: fst, []) xs
    in aux delimiter ([], []) lst;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
