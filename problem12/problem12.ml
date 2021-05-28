type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode rles =
    let rec aux acc = function
        | [] -> acc
        | h :: t ->
                match h with
                | One x -> aux (x :: acc) t
                | Many (c, y) -> aux
                                    ((let rec loop n v a = if (n > 0) then loop (n-1) v (v :: a)
                                         else a @ acc in loop c y [])) t
    in
        List.rev (aux [] rles);;
