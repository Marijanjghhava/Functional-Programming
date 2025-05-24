(*Factorial*)
let factorial n =
  let rec aux n acc = function
  if n <=1 then acc
  else aux (n -1) * (n * acc)
in
aux n 1;;

(*Sum of a list*)

let rec sum list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux ( acc + h ) t
in 
aux 0;;

(*length*)
let length list =
    let rec aux acc = function 
    | [] -> acc
    | h :: t -> aux ( acc + 1) t
in
aux 0;;

(*Reverse*)
let reverse list =
    let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t 
in
aux [] list;;







(**************************************************
*******************Teams Material******************
***************************************************)


(*PROBLEM 1: 
Check which of the following functions are tail recursive*)

(*function 1:*)
let rec f a =  (*a is the list of integers*)
  match a with 
  | [] -> a 
  | x::xs -> (x + 1) :: f xs (*returns a new list where each element is incremented by 1*)

(*ANSWER:
Not tail recursive. after recursive call f xs it still has to do :: operation.*)

(*Tail recursive version:*)
let f a =
  let rec aux acc = function
  | [] -> List.rev acc
  | h :: t -> aux ( h + 1) :: acc t
in
aux [] a;;



(*function 2:*)
let rec g a b =
  if a = b then 0
  else if a < b then g (a + 1) b
  else g (a - 1) b 

  (*ANSWER:
   Is Tail Recursive. Recursive call is the last thing executed *)



(*function 3:*)

let rec h a b c = 
  if b then h a (not b) (c * 2) 
  else if c > 1000 then a 
  else h (a + 2) (not b) c * 2

  (*ANSWER: 
  Not Tail Recurive.*)




(*PROBLEM 2: 
Write tail recursove versions of the following functions*)

(*function1: *)

let rec fac n =
  if n < 2 then 1
  else n * fac (n - 1)


let fac n =
  let rec fac n acc = function
  if n < 2 then acc 
  else aux (n - 1) * (n * acc)
in aux n 1;;

(*function 2: *)

let rec remove a = function
  | [] -> []
  | x :: xs -> if x = a then remove a xs
               else x :: remove a xs


let remove a list = 
  let rec aux  acc = function 
  | [] -> List.rev acc
  | h :: t -> 
    if h = a then aux acc t
    else aux (h :: acc) t
  in
  aux [] list


(*function 3:*)
let rec partition f l =
  match l with
  | [] -> [], []
  | x :: xs ->
      let a, b = partition f xs in
      if f x then x :: a, b
      else a, x :: b



let partition f l =
  let rec aux acc l (a, b) = function
  | [] -> (a , b)
  | h :: t -> aux t ( if f h then h :: a, b else a, h :: b)
in
let r = aux l ([], []) in
List.rev (first r), List.rev (second r);;












  

