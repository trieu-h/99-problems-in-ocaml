let rec length xs =
    match xs with
    | [] -> 0
    | _ :: tl -> 1 + length tl;;
