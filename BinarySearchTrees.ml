(*1.type tree.
Define a suitable data type for binary trees that store integers*)

type tree =
 | Empty
 | Node of int * tree * tree

(*2. let t1.
Define a binary tree t1 containing the values:*)

let t1 =
  Node (6,
    Node (1, Empty, Empty),
    Node (9,
      Node (8, Empty, Empty),
      Node (12, Empty,
        Node (42, Empty, Empty))))



(*3. let to_list.
Returns an ordered list of all values in the tree*)

(*use in-order traversal since the values on the left side are smaller than right side
in-order traversal - left, current, right*)

let rec to_list tree =
  match tree with 
  | Empty -> []
  | Node (value, left, right) -> to_list left @ [v] @ to_list right

(*4. let insert.
Inserts a value, if it exists tree is not modified*)

  let rec insert k tree =
    match tree with 
    | Empty -> (k, Empty, Empty)
    | Node ( v, left, right) ->
      if k = v then tree
      else if k < v then Node(v, insert k left, right)
      else Node(v, left, insert k right)

(*5. let remove.
Remove a value if exists from the tree *)

(*helper function to choose smaller value in case of two children*)
let rec find_min tree =
  match tree with 
    | Empty -> failwith "cant find min of tree"
    | Node( value, Empty, _ ) -> value
    | Node ( _, left, _ ) -> find_min left


  let rec remove k tree =
    match tree with 
    | Empty -> Empty 
    | Node ( v, left, right) ->
      (*first, search for the value to remove*)
      if k < v then 
        Node(v, remove k left, right)
    else if k > v then
      Node(v, left, remove k right)
    else
      (*k = v, this is the node to remove*)
      match left, right with 
      | Empty, Empty -> Empty (* Case 1: it's a leaf*)
      | Empty, _ -> right (*Case 2:  only right child*)
      | _, Empty -> left (* Case 2: only left child*)
      | _ ->
        (*Case 3: two children*)
        let successor = find_min right in
        Node( successor, left, remove successor right) (*replace root with the node and then remove it from where it was*)
        



