(*Tail of a List*)
let rec last list =
  match list with 
  | [] -> None
  | [x] -> Some x
  | h :: t -> last t;;

 (*Last Two Elements of a list*)
  let rec last_two list =
    match list with 
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | h :: t -> last_two t;;

 (*N'th Element of a List*)
    let rec n'th n list =
      match list with 
      | [] -> None
      | h :: t ->if n =0 then Some h else n'th (n-1) t;;

  (*Length of a List*)
  let rec length list =
    match list with 
    [] -> 0
    | h :: t -> 1 + length t;;

    (*tail recursive version*)
    let  length list =
      let rec aux n = function
      | [] -> n
      | h :: t -> aux (n + 1) t
    in 
    aux 0 list;;


  (*Reverse a List*)
  let rec reverse list =
    match list with 
    [] -> []
    |h :: t -> reverse t @ [h];;

  (*tail recursive version*)
  let reverse list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] list;;

  

 


