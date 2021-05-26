let at p xs =
    let rec aux n = function
        | [] -> None
        | h :: t -> if (n == p) then Some(h) else aux (n+1) t
    in aux 0 xs;;
