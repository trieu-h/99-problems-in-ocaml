let rec last_two xs =
    match xs with
    | [] -> None
    | [x; y] -> Some(x, y)
    | _ :: t -> last_two t;;
