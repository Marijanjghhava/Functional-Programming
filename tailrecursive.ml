(************************************************
****************LAB Exercises*********************
*************************************************)

(*Write tail recursive versions of the following functions *)
let rec fac n = if n < 2 then 1 else n * fac (n -1);;

(*Tail recursive version*)
let fac n =
  let rec aux n acc =
  if n <= 1 then acc 
  else aux (n-1) ( n* acc)
in 
 aux n 1;;

 (*Function 2*)
 let rec remove a = function [] -> []
 | x::xs -> if x = a then remove a xs else x::remove a xs

 (*Tail recursive version*)
 let remove a  list=
  let rec aux acc = function
  | [] -> List.rev acc
  | h :: t -> if h = a then aux acc t
  else aux  (h :: acc) t
in 
aux [] list;;

(*Function 3*)
let rec partition f l = match l with [] -> [],[]
        | x::xs -> let a,b = partition f xs in
        if f x then x::a,b else a,x::b

(*Tail recursive version*)
let partition f list =
  let rec aux acc1 acc2 = function
  | [] -> List.rev acc1, List.rev acc2
  | h ::t -> 
    if f h then 
      aux(h :: acc1) acc2 t
   else
    aux acc1 (h :: acc2) t
  in
  aux [] [] list;;





            (***********************************
            **More Exercises on tail recursion**
            ************************************)


  (*Sum of a list*)
let sum list =
  let rec aux acc = function 
  | [] -> acc
  | h :: t ->  aux (acc + h) t
in
aux 0;;

(*Length of a list*)
let length list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (acc + 1) t
in 
aux 0;;

(*Reverse a list*)
let reverse list =
let rec aux acc = function
|[] -> acc
| h :: t -> aux (h :: acc) t
in 
aux [] list;;

(*Tail recursive efunction to compute the sum of squares*)
let sum_of_squares list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (acc + h*h) t
in
aux 0;;


