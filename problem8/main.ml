let rec compress l =
    match l with
    | fst :: snd :: rest -> if (fst = snd) then compress (fst :: rest) else fst :: compress (snd :: rest)
    | x -> x ;;
