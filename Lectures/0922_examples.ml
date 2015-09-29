(*

1) Define a curried function prependTo that satisfies:

   let prependTo : 'a list -> 'a list -> 'a list = TODO

   prependTo [] l = l
   prependTo [1;2;3] [4;5;6] = [4;5;6;1;2;3]

   let f = prependTo [1] 
   in f [2;3] @ f [4;5]
    = 
   [2; 3; 1; 4; 5; 1]

 *)

let prependTo l2 l1 = l1 @ l2;;
let _ = prependTo [2;3] [1;2];;

let prependTo : 'a list -> 'a list -> 'a list = 
  fun l2 l1 -> l1 @ l2;;

let prependTo : 'a list -> 'a list -> 'a list = 
  fun l2 -> fun l1 -> l1 @ l2;;

(* Difference between "fun" and "function" 
 *)

let listType =
  function [] -> "nil"
    | _ :: _  -> "cons"
 ;;

let listType = fun l ->
  match l with
    | [] -> "nil"
    | _ :: _  -> "cons"
 ;;


let add x y = x + y;;
let add5 = add 5;;
let _ = add 3 3;;
let _ = add5 1;;


(*
   Q: What does curried mean?
   
   - Pass arguments one at a time, instead of in a tuple.
   - Higher-order functions. Each time we pass in one argument,
     we get a new function.
   - Allows a function to be applied to some, but not all, arguments.
     Very useful in FP!
 *)


(* 

2) Use map to define a curried function mapBinOp that takes a (also
   curried) binary function and a list of pairs, and applies the
   function to the components of each pair:

 *)

let map = List.map;;

let mapBinOp : ('a -> 'b -> 'c) -> ('a * 'b) list -> 'c list = 
  fun f l ->
  map (fun (x,y) -> f x y) l
;;

let _ = mapBinOp (fun x y -> x+y) [(1,2); (3,4); (5,6)] = [3;7;11]

let _ = mapBinOp prependTo [([1],[2;3]); ([4;5],[6;7])] = [[2;3;1]; [6;7;4;5]]

let _ = mapBinOp (+) [(1,2); (3,4); (5,6)] = [3;7;11]

(* 

Rules:
  map f []      = []                   (* Nil rule *)
  map f (x::xs) = f x :: map f xs      (* Cons rule *)

   mapBinOp (+) [(1,2); (3,4); (5,6)]
 = map (fun (x,y) -> (+) x y) [(1,2); (3,4); (5,6)]
 = map (fun (x,y) -> x + y) [(1,2); (3,4); (5,6)]
 = map (fun (x,y) -> x + y) ((1,2) :: (3,4) :: (5,6) :: [])
 = ((fun (x,y) -> x + y) (1,2))
     :: 
   map (fun (x,y) -> x + y) ((3,4) :: (5,6) :: [])
 = (1 + 2)
     :: 
   map (fun (x,y) -> x + y) ((3,4) :: (5,6) :: [])
 = ...
 = (1 + 2) :: (3 + 4) :: (5 + 6) :: []
 *)

let myOwnShortcut = (+);;

let _ = (-);;
let _ = ( * );;


let uncurriedAdd = (fun (x,y) -> x + y);;
let curriedAdd = (fun x y -> x + y);;

let curry   : (('a * 'b) -> 'c) ->
              ('a -> 'b -> 'c)
   = fun (f : ('a * 'b) -> 'c) ->

     (* Return value :  ('a -> 'b -> 'c) *)
     fun (x : 'a) -> fun (y : 'b) ->
     f (x,y)
         
let uncurry : ('a -> 'b -> 'c) ->
        (('a * 'b) -> 'c)
   =
   TODO
   
(* uncurriedAdd = uncurry curriedAdd *)
(* curriedAdd = curry uncurriedAdd *)


let mapBinOp : ('a -> 'b -> 'c) -> ('a * 'b) list -> 'c list = 
  fun f l ->
  map (uncurry f) l
;;

let uncurriedAdd = (fun (x,y) -> x + y);;
let add5 = curry uncurriedAdd 5;;
let _ = add5 6;;

(*
map : applies a function to each element of a list (returns a new list)

filter: takes a "predicate" and a list, returns a new list
        containing the elements that satisfy the predicate 
 *)

let filter = List.filter;;
let _ = filter (fun x -> x > 3) [4;2;5;3] = [4;5]

(* Example: Using filter to test if a list contains some value.

    contains 'c' ['a';'b';'c'] = true
    contains 'd' ['a';'b';'c'] = false

 *)

let contains e l =
  match (filter (fun x -> x = e) l) with
  | _::_ -> true
  | []   -> false

(* Exercise: implement contains as a recursive function.
 *)

let rec filter p l =
  match l with
  | [] -> []
  | hd :: tl when p hd -> hd :: filter p tl
  | _  :: tl           -> filter p tl

let rec filter p l =
  match l with
  | [] -> []
  | hd :: tl ->
    let tl' = filter p tl in
    if p hd
    then hd :: tl'
    else tl'

let rec filter p l =
  match l with
  | [] -> []
  | hd :: tl ->
    if p hd
    then hd :: filter p tl
    else filter p tl

(* CELEBRATE *)
