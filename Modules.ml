module type Set = sig
  type t
  val to_string : t -> string
end

module type Map = sig
  type key
  type value
  type t
  val empty : t
  val set : key -> value -> t -> t
  val get : key -> t -> value
  val get_opt : key -> t -> value option
  val to_string : t -> string
end



(*1.  Implement a module StringSet of signature Set to model sets
of strings.*)

module StringSet : Set with type t = string = struct
  type t = string
  let to_string s = "\"" ^ s ^ "\""
end

(*2. Define a signature OrderedSet that extends the Set signature
by a compare function with the usual type.*)

module type OrderedSet = sig
 include Set
 val compare : t -> t -> int
end



(*3. Implement a functor BTreeMap that realizes the Map signature
and uses a binary tree to store key-value-pairs. The functor
takes key and value sets as arguments *)

module BTreeMap (Key :: OrderedSet) (Value :: Set) :: Map
 type key = key.t
 type value = value.t
 type t = Empty | Node of t* key * value * t


 let empty = Empty
 
(*set - key is now mapped to value*)
 let rec set k v tree  =
  match tree with
  | Empty -> Node(k, v, Empty, Empty)
  | Node (left, k', v', right ) ->  
    let compare = k.compare k k' in
    if compare = 0 then (*if current key and input key are the same*)
      Node(left, k, v, right)
    else if compare < 0 then
      Node(set k v left, k', v', right) (*go left*)
    else 
      Node (left, k', v', set k v right) (*go right*)


  (*get - gets the value for the given key, Not_found if no such key exists*)
  let rec get k v tree =
    match tree with 
    | Empty -> raise Not_found
    | Node (left, k', v', right) ->
      let compare = k.compare k k' in
      if compare =0 then v'
      else if compare > 0 then get k right
      else get k left 


(*get_opt - fets the value for the given key or None if the key doesnt exist*)
  let rec get_opt k v tree =
    match tree with 
    | Empty -> None
    | Node (left, k', v', right) ->
      let compare = k.compare k k' in
      if compare =0 then v'
      else if compare > 0 then get k right
      else get k left 

(*to_string - string representation for the mapping*)
  let to_string m =
    List.map (fun (k,v) -> K.to_string k ^ " -> " ^ V.to_string v) (to_list m)
    |> String.concat ", "
    |> Printf.sprintf "{ %s }"
end

(*4. Implement a StringSet and an ordered IntSet module.*)
 
module StringSet : Set with type t = string = struct
  type t = string
  let to_string s = "\"" ^ s ^ "\""
end

module Intset : Set with type t = int = struct
  type t = int
  let compare = compare
  let to_string = string_of_int
end


