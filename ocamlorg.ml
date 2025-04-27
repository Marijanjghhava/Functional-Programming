(*Tail of a list*)

let rec last list =
  match list with 
  [] -> None
  | [x]  -> Some x
  | h :: t -> last t;; 

(* Last two elements of a List*)
 let rec last_two list =
  match list with 
  [] -> None
  | [x] -> None
  | [x; y] -> Some (x , y)
  | h :: t -> last_two t;;


