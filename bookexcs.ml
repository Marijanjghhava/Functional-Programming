(**********************************)
(*******NAMES AND FUNCTIONS********)
(***********************************)

(*1.write a function which multiplies a given number by ten*)

let times_ten x = x * 10;;
let result = times_ten 5;;

Printf.printf  "%d" result;;

(*2.write a function which returns true if both of its arguments are non-zero,
 and false otherwise.*)

let nonzero x y =
    x <>0 && y <> 0;;


(*3.Write a recursive function which, given a number n, 
calculates the sum 1 + 2 + 3 + +n. *)

let rec sum n =
    if n = 1 then 1 else n + sum(n -1);;

    let result = sum 5;;
    Printf.printf "%d" result;;


(*4.Write a function power x n which raises x to the power n*)

let rec power x n =
    if n = 0 then 1 else 
        (if n =1 then x else 
        x *  power x(n -1));;



(*5.Write a function isconsonant which, given a lower-case character in the range 'a'...'z', deter
mines if it is a consonant.*)

let  is_vowel x =
    x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u';;

let is_consonant y = not (is_vowel y);; 

(*******************************)
(*******PATTERN MATCHING********)
(********************************)



(*1.Rewrite the not function from the previous chapter in pattern matching style.*)

(*not function*)
let not x = 
    if x then false else true;;

(*rewritten as pattern matching*)

let not x = 
    match x with
    true -> false 
    | false -> true;;

(*2.Use pattern matching to write a recursive function which, 
given a positive integer n, returns the sum of all the integers
 from 1 to n*)

 let rec sum n =
    match n with 
    1 -> 1
    | _ -> n + sum(n-1);;

(* 3. Use pattern matching to write a function which, 
given two numbers x and n, computes xn.*)

let power x n =
    match n with 
    0 -> 1
    | 1 -> x
    | _ -> x *  power x(n -1);;


(* 6. There is a special pattern x..y to denote continuous ranges of characters, 
for example 'a'..'z' will match all lowercase letters. 
Write functions islower and isupper to decide on the case of a given letter.*)

let islower c =
    match c with 
    'A' .. 'Z' -> false
    | _ -> true;;

let issuper d =
    match d with 
    'a' .. 'z' -> true
    | _ -> false;;


(************************************************)
(*******************LISTS************************)
(************************************************)


(*1. Write a function evens which does the opposite to odds, 
returning the even numbered elements in a list. *)

let rec evens x =
    match x with
    | [] -> []
    | [_] -> []  (* one element: no even element to pick, so return empty list *)
    | a :: _ :: t -> a :: evens t
  ;;
  

(*more optimal way*)
let rec evens x =
    match x with 
    _ :: b :: t -> b :: evens t (*drop one, keep one*)
    | _ -> [];; (*otherwise, no more to drop*)


(* 2. Write a function count_true which counts the number of true elements in a list. *)
let rec count_true x =
    match x with 
    [] -> 0
    | true :: t -> 1 + count_true t
    | false :: t -> 0 + count_true t;;


(*Write a function count_zero that counts the number of zeros in a list.*)

let rec count_zero x =
    match x with 
    [] -> 0
    | h :: t -> if h=0 then  1 + count_zero t else count_zero t;;

(*Write a function count_negative that counts how many negative numbers are in a list.*)
let rec count_negative x =
    match x with 
    [] -> 0
    |h :: t -> if h < 0 then 1 + count_negative t else count_negative t;;


(*3.Write a function which builds palindrome of a list.*)
let rec reverse x =
    match x with 
    [] -> []
    | h :: t -> reverse t @ h;;

(*4.Write a function drop_last which returns all but the last element of a list.*)
let rec drop_last x =
    match x with 
    [] -> []
    | [_] -> []
    | h :: t -> h ::  drop_last t;;


(*5.Write a function member which returns true if an element exists in a list.*)

let rec member x list =  (*we are given a value of x and a list*)
    match list with
    [] -> false (*base case*)
    | h :: t -> if h = x then true else member x t(*recursive case*)

(*another soultion*)

let rec member x list=
match list with 
[] -> false
| h :: t -> h=x || member x t (*checks if the x is the head, if not searches in tail*)

(*6.Write a function make_set, witch returns the same list without any duplicates*)

let rec make_set list =
    match list with 
    [] -> []
    | h :: t -> if member h (make_set t) then make_set t else h :: make_set t;;





 