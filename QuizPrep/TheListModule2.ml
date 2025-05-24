(*1. Sum of squares of the integers in the list*)

let squaresum1 list =
  List.fold_left (fun x y -> x+y*y) 0 list

let squaresum2 list =
   List.fold_right( fun x y -> x*x+y) list 0

(*2. Converts all ints to floats*)

let float_list list =
  List.map float_of_int list

(*3. Builds a string representation of the given list*)

let to_string list =
  "[" ^ String.concat "; " (List.map string_of_int) ^ "]"

(*4. partitioons all even values to the front*)

let part_even lsit =
  let even = List.filter(fun x -> x mod 2 = 0) list in
  let odd = List.filter(fun x -> x mod 2 <> 0 ) list in
  even @ odd
