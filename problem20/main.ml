let remove list k =
    if k > List.length list
    then failwith "Index out of bound"
    else
        let rec _remove counter acc = function
        | [] -> List.rev acc
        | x :: xs -> let lst = if (counter = k) then acc else (x::acc)
                     in _remove (counter + 1) lst xs
        in _remove 0 [] list;;

(* remove [1;2;3;4;5;6] 3;; *)
