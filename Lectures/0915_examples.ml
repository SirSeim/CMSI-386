(*

Recursive datatypes:

great for defining infinite sets of values.
  containers: lists, sets, key-value maps

Lists need not be built in! 

 *)

type intList = Empty | Node of int * intList;;

(* 
Empty ~ []
Node ~ ::

[1;2] = 1::2::[] ~ Node (1, Node (2, Empty))
 *)

let rec sumList l =
  match l with
  | Empty -> 0
  | Node(x,xs) -> x + sumList xs
;;

type 'a myList = Empty | Node of ('a * 'a myList);;

Node("hi",Empty);;

Node(1,Empty);;

type intOrStringList = 
    Empty
  | IntNode of (int * intOrStringList)
  | StringNode of (string * intOrStringList)
;;

let l = IntNode (1,StringNode ("hi", Empty));;

type 'a append_list =
   Empty
 | Single of 'a
 | Append of ('a append_list * 'a append_list)
;;

let rec sumList l =
  match l with
  | Empty -> 0
  | Single x -> x
  | Append(l1,l2) -> sumList l1 + sumList l2
;;

let suml = Append (Single 5, Single 6);;

let l1 = Single 1;;
let l1' = Append(Single 1, Empty);;
let _ = assert(l1 = l1');;

let l123 = Append(Append (Single 1, Single 2), Single 3);;
let l123' = Append(Single 1, Append (Single 2, Single 3));;
let _ = assert(l123 = l123');;

(* Exercise: 
 *)
let rec equal ((l1,l2) : 'a append_list * 'a append_list) : bool =
  let rec to_list (l : 'a append_list) : 'a list =
   match l with
    | Empty -> []
    | Single e -> [e]
    | Append(l1,l2) -> to_list l1 @ to_list l2
  in to_list l1 = to_list l2


(* Hint: remember what I said the difference between append_list and OCaml's list is *)

type tree = Leaf 
          | TreeNode of tree * int * tree
;;

let t1 = TreeNode (Leaf, 1, Leaf);;
let t3 = TreeNode (Leaf, 3, Leaf);;
let t123 = TreeNode (t1, 2, t3);;

let t5 = TreeNode (Leaf, 5, Leaf);;
let t7 = TreeNode (Leaf, 7, Leaf);;
let t567 = TreeNode (t5,6,t7);;

(*
Tests:
  size Leaf = 0
  size t5 = 1
  size t567 = 3

Rules:
  size Leaf = 0
  size (TreeNode (left,element,right)) = 1 + size left + size right

Check:
  size t5 
= size (TreeNode (Leaf, 5, Leaf))     (* By definition of t5 *)
= 1 + size Leaf + size Leaf           (* By the TreeNode rule *)
= 1 + 0 + 0                           (* By the Leaf rule twice *)
= 1 (* by math *)

 *)  

let rec size (t:tree) : int =
  match t with
  | Leaf -> 0
  | TreeNode (l,e,r) -> 1 + size l + size r
;;

(* 
  Binary Search Tree:

  a tree satisfying the property:
  
  for every TreeNode (l,x,r):
    x greater than or equal to every element in l
    x less than every element in r

  Note: A Leaf is a binary search tree containing 0 elements.

  Useful for storing sets of elements with fast lookup on average.

 *)

tree_to_list Leaf = [];;
tree_to_list (TreeNode (Leaf,1,Leaf)) = [1];;
tree_to_list (TreeNode (t123,4,t567)) = [1;2;3;4;5;6;7];;