type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten nodes =
  match nodes with
  | [] -> []
  | One x :: xs -> x :: flatten xs
  | Many l :: xs ->
      let rec append l1 l2 =
        match l1 with
        | h :: t -> h :: append t l2
        | [] -> l2
      in append (flatten l) (flatten xs);;
