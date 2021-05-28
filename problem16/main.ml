(* drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;; *)

let drop l n =
    let rec aux acc n c = function
    | [] -> acc
    | x :: xs -> if n > 0 then aux (x :: acc) (n-1) c xs else aux acc c c xs
    in aux [] n n;;
