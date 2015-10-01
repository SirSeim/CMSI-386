(* Module scope *)

let _ = List.map;;
let map = 34;;
let _ = List.map;;
let _ = map;;

(* CELEBRATE *)

(* Exceptions 

a structured way to signal an error to callers
  - allows the caller to choose how to deal with the error
  - can't be confused with an expected return value

We've seen some examples of handling error cases in OCaml:
  - So far, we've used the option type. None signals an error.
  - last: return None if list is empty.


*)


let rec last l =
  match l with
  | []     -> None
  | hd::[] -> Some hd
  | _::tl  -> last tl

let _ = last [];;
let _ = last [1];;

(*

  - get from a dictionary: return None if key not found
  - advantage of option types:
    makes possibility of error explicit
  - disadvantage:
    everyone is forced to deal with the error
      - have to always "unpack" the option 
        to use the return value
      - that can be a good thing, but it also
        has disadvantages:
        - may not be the right caller to handle the
          error, so would just pass it on
     - divideByLast divides a number by last
       element in a list.

 *)

let divideByLast (n:int) (l:int list) =
  match last l with
  | Some d when d != 0 -> Some (n/d)
  | _                  -> None

 (* imprecise, tedious, code unreadable *)

(*
- Example: get all keys (in a list) from a dictionary.
- if any get fails, then fail.
 *)

let rec getAll (keys : 'a list) (d : ('a, 'b) dict) : 
               ('b list) option =
  match keys with
  | [] -> Some []
  | k::keys' -> match (get k d, getAll keys' d) with
                | (Some v, Some vs) -> Some (v::vs)
                | _                 -> None

(*
- ugly! the purpose of the code (what it's trying to do) is
  completely obscured by handling the possibility of error.
 *)

(*
instead, terminate the function abnormally by 
raising an exception:
 - halts execution of the function and returns to caller
 - caller can "catch" the exception to recover
 - or caller can do nothing, in which case control 
   returns to *its* caller
 - if nobody in the call stack catches it, the
   program crashes
 *)

let _ = 5;;

exception Last_of_Nil;;

(* - declares a new value of type exn

like adding a case to a user-defined type

type exn = ... | Last_of_Nil

but exn is *extensible*

 *)

let rec last (l : 'a list) : 'a =
  match l with
  | []    -> raise Last_of_Nil
  | [e]   -> e
  | _::tl -> last tl


let divideByLast n l = n / last l;;

let _ = divideByLast 5 [1;2;3];;
let _ = divideByLast 5 [];;
let _ = divideByLast 5 [4;3;2;1;0];;

(* catching exceptions in ocaml *)

let safeDivideByLast n l =
  try
    divideByLast n l
  with
  | Last_of_Nil -> n
  | Division_by_zero -> max_int (* approximately infinity *)

let _ = safeDivideByLast 5 [];;
let _ = safeDivideByLast 5 [3;2;1;0];;

let safeDivideByLast n l =
  try
    divideByLast n l
  with
  | Last_of_Nil -> n
  ;;

let _ = safeDivideByLast 5 [3;2;1;0];;

let safeDivideByLast n l =
  try
    divideByLast n l
  with
  | Last_of_Nil -> "empty list"
  | Division_by_zero -> "divide by 0"


(*

Type checker requires that the type of each exception handler
expression is the same as that of the tried expression.


Suppose we re-implement dictionaries so that get 
throws an exception Not_found if the key is not
found (instead of using option).

  get : 'a -> ('a, 'b) dict -> 'b

Re-implement getAll.
  - should throw Not_found if any key is not found

 *)

let rec getAll (keys : 'a list) (d: ('a, 'b) dict) : 
               'b list =
  match keys with
  | []       -> []
  | k::keys' -> get k d :: getAll keys' d 

let getAll keys d = map (fun k -> get k d) keys


let getAll keys d = 
  map (fun k -> try (get k d) with Not_found -> 5) keys

exception Key_not_found of string;;

let rec getAll (keys : 'a list) (d: ('a, 'b) dict) : 
               'b list =
  match keys with
  | []       -> []
  | k::keys' -> try (get k d :: getAll keys' d) with
                | Not_found -> raise Key_not_found k

(* CELEBRATE *)

exception Key_not_found of (string * string);;

let safeDivideByLast n l =
  try
    divideByLast n l
  with
  | _ -> -1


exception Exception_with_msg of string;;

let safeDivideByLast n l =
  try
    divideByLast n l
  with
  | _ -> raise (Exception_with_msg "something bad happened")


