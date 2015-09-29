type tree = Leaf | Node of tree * int * tree

(*

Binary search tree:

For every Node (l,x,r):
  x is greater than or equal to every element in l
  x is less than every element in r

Equivalently:

  Leaf is a binary search tree.
  Node(l,x,r) is a binary search tree if:
    l and r are binary search trees, and
    x is greater than or equal to every element in l, and
    x is less than every element in r

 *)

  let t1 = Node (Leaf, 1, Leaf);;
  let t3 = Node (Leaf, 3, Leaf);;
  let t123 = Node (t1, 2, t3);;

  let t5 = Node (Leaf, 5, Leaf);;
  let t7 = Node (Leaf, 7, Leaf);;
  let t567 = Node (t5,6,t7);;

  let t1to7 = Node (t123, 4, t567);;


(*
Tests:
    insert (1,Leaf) = 
      Node (Leaf,1,Leaf)
    insert (1,Node(Leaf,1,Leaf)) = 
      Node (Node (Leaf,1,Leaf), 1, Leaf)
    insert (2,Node(Leaf,1,Leaf)) = 
      Node (Leaf, 1, Node (Leaf, 2, Leaf))
 *)

let rec insert ((x,t) : int * tree) : tree =
  match t with
  | Leaf   -> Node (Leaf, x, Leaf)
  | Node (left,value,right) ->
     if x <= value
     then Node (insert (x,left), value, right)
     else Node (left, value, insert (x,right))
;;

let _ = insert (1,Leaf);;
let _ = insert (3,t123);;

(*
Node (
  Node (Leaf, 1, Leaf), 
  2, 
  Node (
   Node (Leaf, 3, Leaf), 
   3, 
   Leaf))
 *)

(* Exercise: experiment with:

let rec insert ((x,t) : int * tree) : tree =
  match t with
  | Leaf   -> Node (Leaf, x, Leaf)
  | Node (left,value,right) ->
     if x <= value
     then insert (x,left)
     else insert (x,right)
;;

 *)

(* Use BSTs to sort a list. *)
let sort(l : 'a list) : 'a list =
  let rec list_to_tree l =
  match l with
  | [] -> Leaf
  | first::rest -> insert (first, list_to_tree rest)
  in
  let rec tree_to_list t =
  match t with
  | Leaf -> []
  | Node(l,value,r) -> 
     tree_to_list l @ [value] @ tree_to_list r
  in
  tree_to_list (list_to_tree l)
;;


(* First-class functions

 Functions are data. 
 You can assign them to variables.
 You can take them as input to functions.
 You can return them from functions.
 Anonymous functions.

 *)

let square x = x * x;;

let twice (f, x) = f (f x);;

let _ = twice (square, 3);;

let _ = twice ((fun x -> x * x), 3);;

(*
let square x = x * x;;

let square = fun x -> x * x;;
 *)

let square = fun x -> x * x;;
let square2 = square;;
let _ = square2 5;;

(* Functions as results *)

let returnsAdd() =
   let add (x,y) = x+y
   in add;;

let _ = returnsAdd()(4,5);;

let returnsAdd _ =
   let add (x,y) = x+y
   in add;;

let _ = returnsAdd()(4,5);;
let _ = returnsAdd[](4,5);;

let returnsAdd() =
   fun (x,y) -> x+y
;;

let a = returnsAdd();;
let _ = a(4,5);;

let mkAdd x = fun y -> x+y;;
  
let add5 = mkAdd 5
let _ = add5 17;;  

let x = 6;;
let _ = add5 17;;  

let add6 = mkAdd 6;;
let _ = add6 6;;
let _ = add5 6;;

let _ = add5 == add6;;
let add5' = mkAdd 5;;

let _ = add5 == add5';;

let _ = mkAdd;;

let rec sumList l =
  match l with
  | [] -> 0
  | f::r -> f + sumList r

let rec sumList = 
  function l ->
  match l with
  | [] -> 0
  | f::r -> f + sumList r
  
(* 
   int -> int -> int
=  int -> (int -> int)

Not the same as:
   (int -> int) -> int

 *)

let foo :    (int -> int) -> int =
  function f -> f 5
;;

let _ = foo add5;;

let add (x,y) = x+y;;

let add = function x -> function y -> x+y;;

let _ = add 5 7;;

let add5 = add 5;;
let _ = add5 7;;

let add x y = x+y;;

let _ = [add5 1; add5 2; add5 3];;

(* 
Partially applying a function is called "Currying".

Named after Haskell Curry.

 *)

let _ = [add 0; add 1; add 2];;

(*

This is the common OCaml style:
- use currying by default
  - simpler syntactically both for function writers and function callers
- use tuples when you want some data to be always treated as a unit

 *)

let rec incLst l =
  match l with
  | []    -> []
  | x::xs -> (x+1) :: incLst xs;;
                 
let _ = incLst [1;2;3];;

let rec swapLst l =
  match l with
  | []           -> []
  | (x,y)::rest  -> (y,x) :: swapLst rest;;

let _ = swapLst [("hi",1);("hey",2)];;

let map = List.map;;

let incLst = map (fun x -> x+1);;
let _ = incLst [1;2;3];;


let swapLst = map (fun (x,y) -> (y,x));;
let _ = swapLst [("hi",1);("hey",2)];;

let _ = map;;

let rec map f l =
  match l with
  | []    -> []
  | x::xs -> f x :: map f xs

let incLst = map (fun x -> x+1);;
let _ = incLst [1;2;3];;


let swapLst = map (fun (x,y) -> (y,x));;
let _ = swapLst [("hi",1);("hey",2)];;

(*
    Good style to use map where possible, 
    instead of implementing the
    recursion manually.

    Good style to use anonymous functions if
    they are just helpers for
    the main computation that are used once.
 *)

let incBy n lst = map (fun x -> x + n) lst;;

let _ = incBy 5 [1;2;3];;