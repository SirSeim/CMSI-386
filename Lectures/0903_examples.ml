let ls = [1,""] (* Not a 2-element list *)
let ls = [1;""] (* A 2-element list *)

let x= [4;5]
let y = (1+2) :: x
let _ = x (* Functional: x is unchanged *)
let _ = y (* y is a new list *)

let rec sumList lst =
  match lst with
  | [] -> 0
  | hd::tl -> hd + sumList tl

let _ = sumList [1;2;3;4]                

let _ =
  match [1;2;3] with
  | 1::[] -> "[1]"
  | 1::(2::(3::[])) -> "[1;2;3]"
  | _ -> "?"

let rec everyOther l =
  match l with
  | [] -> []
  | [e] -> [e]
  | hd::_::tl ->
     hd :: everyOther tl

let _ = everyOther [1;2;3;4;5]
let _ = everyOther [1;2]
let _ = everyOther [1;2;3;4;5;6;7]
let _ = everyOther []
let _ = everyOther [1]

(* Giving names to intermediate values. *)         
let quadruple x =
  match double x with
  | d -> d + d

(* New expression: let P = E1 in E2
   Like a match expression with only 1 case:

  let P = E1 in E2 
    can be written
  match E1 with P -> E2
 *)
let quadruple x =
  let d = double x
  in d + d

(* Nested lets *)
let _ =
  let x = 3+4 in
  let y = 5+6 in
  x + y

(* Let expressions can be used anywhere
   an expression can *) 
let _ = 3 + (let x = 4+5 in x+6)

(* Pattern [x;y;z] only matches a three-element list *)       
let [x;y;z] = [1;2;3]         

(* Run-time error: expected a three-element list,
   got a two-element list *)
let [x;y;z] = [1;2]

(* These tuples have different types *)     
let _ = (1, "hi", 2.0) 
let _ = (1, "hi")       
let _ = (1,1,1)

(* Pattern matching on tuples *)
let (a,b) = (1,"hi")

(* The pattern must contain the right number of
   elements, or else type error *)        
let (a,b) = (1,2,3)

(* Nested pattern. *)
let ((x,y), h::t) = ((1,2), [3;4])        