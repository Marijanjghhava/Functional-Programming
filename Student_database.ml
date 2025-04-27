type student = {
  first_name : string;
  last_name : string;
  id : int;
  sem : int;
  grades: (int * float) list;

}

type database = student list;;

(*Adds student to the front of the list*)
let insert s db = s :: db;;

(*Traverse the list and see if student matches*)
let rec find_by_id id db =
  match db with 
  [] -> []
  | h :: t -> if h.id = id then [h] else find_by_id id t;;

let rec findbylast_name last_name db =
  match db with 
  [] -> []
  | h :: t -> if h.last_name = last_name
    then h::findbylast_name last_name t
else findbylast_name last_name t;;

(*Remove the student with the given id from the database*)
let rec remove id db =
  match db with 
  [] -> []
  |[_] -> []
  |h :: t -> if h .id =id then t else remove id t;;

(*Counts the num of students in semester*)
let rec count sem db =
  match db with 
  | [] -> 0
  | h :: t -> (if h.sem = sem then 1 else 0) + count sem t;;







