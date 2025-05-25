(*1. Checks if the mapping is empty*)
  
  let is_empty m =
    match m with 
    | [] -> true
    | h :: t -> false

(*2. Returns value of a given key *)

let rec get k m =
  match m with 
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else get k t

(*3. Insert or replace the value for a key *)

let rec put k v m =
  match m with 
  | [] -> [(k, v)]
  | (k', v') :: t -> if k =k' then (k, v ) :: t else (k',v'):: put(k v t)


(*4. Check if the key is in the map*)

  let contains_key k m =
    List.exists (fun (k', _) -> k = k' ) l

(*5. Remove any pair with key k from the list*)

let rec remove k m =
  match m with 
  | [] -> []
  | ( k', v'):: t -> if k = k' then t else (k', v'):: removove k t

(*6.Return a list of all keys in the map*)

let rec keys m =
  match m with 
  | [] -> []
  | (k, _):: t -> k::(keys t)

(*7. Return a list of all values in the map *)

let rec values m =
  match m with 
  |[] -> []
  |(_, v) :: t -< v::(values t)


   