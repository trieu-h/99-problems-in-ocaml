open List

let at (p: int) (xs: 'a list) =
    try List.nth xs p with
       | Failure _ -> None
       | Invalid_argument _ -> None;;
