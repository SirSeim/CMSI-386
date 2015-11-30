
(* Grammar of expressions:
   E ::= N | B | X | E && E | E + E | if E then E else E 
       | let X=E in E

   ex:

    if true && false then 1 else 0
    
    let x = true in x && x

   user-defined types!
 *)

type exp =
   Int of int
 | Bool of bool
 | Var of string
 | And of exp * exp
 | Plus of exp * exp
 | If of exp * exp * exp
 | Let of string * exp * exp
;;

(* if true && false then 1 else 0 *)

let _ = And (Bool true, Bool false);;

let e1 = If (And (Bool true, Bool false),  (* condition *)
             Int 1,   (* then branch *)
             Int 0);; (* else branch *)

(* let x = true in x && x *)

let e2 = Let ("x", Bool true, And (Var "x", Var "x"));;

(* Interpreter : function from expressions to "values".
   Values are "fully-evaluated expressions".

   Grammar for values:
     mlval ::= N | B
 *)

type mlval = IntVal of int | BoolVal of bool;;

(* Now we can define an interpreter for these
   expressions as a recursive function.

   Input: Expression of type exp
   Output: Value of type mlval

   Need to handle each form of expression
   (i.e. each constructor of the type exp).

   Let's start with just integer expressions and
   boolean expressions.
 *)

let rec eval (e : exp) : mlval =
  match e with
  | Int  i -> IntVal i
  | Bool b -> BoolVal b
;;
  
let _ = eval (Int 5);;

(* Extend with And expressions *)

exception EvalError of string;;

let rec eval (e : exp) : mlval =
  match e with
  | Int  i      -> IntVal i
  | Bool b      -> BoolVal b
  | And (e1,e2) -> 
    (match (eval e1, eval e2) with
     | (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
     | _                        -> 
       raise (EvalError "&& excepted two BoolVals")
     )
;;

let _ = eval (And (Bool true, Bool false));;
let _ = eval (And (Bool true, Int 5));;

(* Meta-circularity: (b1 && b2)
   Using OCaml's && to evaluate And expressions.
 *)

(* Handle Plus *)

let rec eval (e : exp) : mlval =
  match e with
  | Int  i      -> IntVal i
  | Bool b      -> BoolVal b
  | And (e1,e2) -> 
    (match (eval e1, eval e2) with
     | (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
     | _                        -> 
       raise (EvalError "&& excepted two BoolVals")
     )
  | Plus (e1,e2) ->
    (match (eval e1, eval e2) with
     | (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
     | _                      ->
       raise (EvalError "+ excepted two IntVals")
    )
;;

(* Meta-circularity: (n1 + n2) *)

let _ = eval (Plus (Int 5, Int 7));;      (* IntVal 12 *)
let _ = eval (Plus (Int 5, Bool true));;

(* If expressions: *)

let rec eval (e : exp) : mlval =
  match e with
  | Int  i      -> IntVal i
  | Bool b      -> BoolVal b
  | And (e1,e2) -> 
    (match (eval e1, eval e2) with
     | (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
     | _                        -> 
       raise (EvalError "&& excepted two BoolVals")
     )
  | Plus (e1,e2) ->
    (match (eval e1, eval e2) with
     | (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
     | _                      ->
       raise (EvalError "+ excepted two IntVals")
    )
  | If (cond,thenE,elseE) ->
    (match eval cond with
     | BoolVal b -> if b then eval thenE else eval elseE
     | _ -> raise (EvalError "if expected a BoolVal")
    )
;;

(* meta-circularity: using ocaml's if expressions
   to evaluate our If *)

let _ = eval (If (Bool true, Int 5, Bool true));;
let _ = eval (If (Int 0, Int 5, Bool true));;

(*  Environment maps variable names to values.
 *)

type mlenv = (string * mlval) list;;

(* CELEBRATE *)

let rec get (k : string) (env : mlenv) : mlval =
  match env with
  | [] -> raise (EvalError ("var not found: " ^ k))
  | (k',v)::env' -> if k' = k then v else get k env'
;;

let env = [("b", BoolVal true); ("n", IntVal 5)];;
let _ = get "b" env;;
let _ = get "n" env;;
let _ = get "a" env;;

(* Extend our interpreter to handle variables.
   Add an environment argument to our interpreter.
 *)

let rec eval (env : mlenv) (e : exp) : mlval =
  match e with
  | Int  i      -> IntVal i
  | Bool b      -> BoolVal b
  | Var x       -> get x env
  | And (e1,e2) -> 
    (match (eval env e1, eval env e2) with
     | (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
     | _                        -> 
       raise (EvalError "&& excepted two BoolVals")
     )
  | Plus (e1,e2) ->
    (match (eval env e1, eval env e2) with
     | (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
     | _                      ->
       raise (EvalError "+ excepted two IntVals")
    )
  | If (cond,thenE,elseE) ->
    (match eval env cond with
     | BoolVal b -> if b then eval env thenE else eval env elseE
     | _ -> raise (EvalError "if expected a BoolVal")
    )
;;

let _ = eval [("b", BoolVal true)] (Var "b");;
let _ = eval [("b", BoolVal true)] (Var "a");;

(* Let expressions.

   Make sure to implement *static scoping*:
   - variable always refers to the nearest enclosing
     definition.

     let x = 3 in (let x = 4 in x) + x

   - functions are in the homework. can be tricky.
   - can define a function in E1, every call to it
     must be evaluated in E1. 
   - even if the call is within some other environment E2.

   - Hw3: function values need to be stored with their
     defining environment.
 *)


let rec eval (env : mlenv) (e : exp) : mlval =
  match e with
  | Int  i      -> IntVal i
  | Bool b      -> BoolVal b
  | Var x       -> get x env
  | And (e1,e2) -> 
    (match (eval env e1, eval env e2) with
     | (BoolVal b1, BoolVal b2) -> BoolVal (b1 && b2)
     | _                        -> 
       raise (EvalError "&& excepted two BoolVals")
     )
  | Plus (e1,e2) ->
    (match (eval env e1, eval env e2) with
     | (IntVal i1, IntVal i2) -> IntVal (i1 + i2)
     | _                      ->
       raise (EvalError "+ excepted two IntVals")
    )
  | If (cond,thenE,elseE) ->
    (match eval env cond with
     | BoolVal b -> if b then eval env thenE else eval env elseE
     | _ -> raise (EvalError "if expected a BoolVal")
    )
  | Let (nm, defn, body) ->
    let defnVal = eval env defn in
    let env' = (nm,defnVal)::env in
    eval env' body
;;

(* let x = true in x && false *)

let _ = eval [] (Let ("x",
                      Bool true,
                      And (Var "x", Bool false)));;

(* let x = 1 in (let x = 2 in x) + x *)
let _ = eval [] (Let ("x",
                      Int 1,
                      Plus 
                       (Let ("x", Int 2, Var "x"),
                        Var "x")));;

(* CELEBRATE *)
