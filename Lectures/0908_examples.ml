(* 
Example:
Find the prime numbers less than some number N.

Three functions:
  divides: check if a number divides another.
  prime: check if a number is prime
  primes: tie it all together.
*)


let divides((d,n) : int * int) : bool =
  n mod d = 0

let _ = assert(divides(3,9))
let _ = assert(not (divides(3,10)))

let prime(n:int) : bool =
  let rec noDivisorsGeq(d,n) : bool =
    if d >= n
    then true (* tried everything *)
    else if divides(d,n)
    then false (* found a divisor *)
    else noDivisorsGeq(d+1,n) (* keep looking *) 
  in noDivisorsGeq(2,n)

let _ = assert(not (prime 6))
let _ = assert(prime 1) 
let _ = assert(prime 2)                  
let primes (max:int) : int list =
  let rec primesGeq(l:int) : int list =
    if l >= max
    then []
    else if prime l
    then l :: primesGeq (l+1)
    else primesGeq (l+1)
  in primesGeq 2

(* User-defined types *)

type sign = Pos | Neg | Zero
  (* Pos, Neg, Zero are constants of type sign *)  

(* All you can do with Pos,Neg,Zero is pass them around,
   test for equality, and pattern-match
 *)
   
let signOf(n:int) : sign =
  match n with
  | 0            -> Zero
  | _ when n < 0 -> Neg
  | _            -> Pos

let signToInt s =
  match s with
  | Zero -> 0
  | Pos  -> 1
  | Neg  -> -1

(* Types can have associated data *)
type point = Cartesian of int * int       

  (* Cartesian is a "constructor".
   *)

(* like
struct point {
  int i,j;
}         
 *)

let negate (Cartesian(x,y)) =
  Cartesian(-x,-y)

type point = Point of int * int

let pair2point (x,y) = Point (x,y)
let point2pair (Point (x,y))
  = (x,y)

type option = None | Some of int                  

let head (l:int list) =
  match l with
  | [] -> None
  | hd::_ -> Some hd

type 'a option = None | Some of 'a

let head (l:'a list) : 'a option =
  match l with
  | [] -> None
  | hd::_ -> Some hd

  (* Now head is defined on all lists. Never throws an exception *)

let safeDiv(x,y) =
  if y = 0
  then None
  else Some (x/y)

let _ = assert(safeDiv(2,0) = None) 