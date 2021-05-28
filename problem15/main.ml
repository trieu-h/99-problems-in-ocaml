(* replicate ["a"; "b"; "c"] 3;; *)

let replicate l n =
    let rec loop n v acc =
        if (n > 0) then loop (n-1) v (v :: acc) else acc
    in
    let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> aux (loop n h acc) t
    in
    aux [] l;;
