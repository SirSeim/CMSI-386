(*
fold_right takes three arguments:
  1) a function f defining how an element is
     incorporated into a result
  2) a list of elements
  3) an initial result r

  f takes two inputs:
    the head of a list, and
    the result of folding over the tail.

    incorporates the head into the result.

  Rules for fold_right:

  Nil rule:
    fold_right f []       r = r
  Cons rule:
    fold_right f (hd::tl) r = f hd (fold_right f tl r)

  Note: Nil rule is base case (no recursive call to 
        fold_right)
    Cons rule is recursive case (recursive call).
    what changes on the right hand side of the Cons rule?
      - call to f
      - list argument to fold_right gets smaller
    - from (hd::tl) to tl.
      "one call to f per element in the list"

 *)

let rec fold_right f l r =
  match l with
  | []     -> r
  | hd::tl -> f hd (fold_right f tl r) 

(* Exercise: convince yourself that this obeys the rules *)

(* Example: reverse using fold_right *)

let reverse l = fold_right f l r

  (* incomplete: need to replace "f" and "r" *)

(* Let's use equational reasoning to figure out what f and r should be.
 *)

    reverse []
  = fold_right f [] r     (* Definition of reverse *)


(*  Nil rule:
    fold_right f []       r = r
 *)
 
  = r   (* By the nil rule *)
  = []

  (* CELEBRATE: r = [] *)

  (* How about "f"? 
     Assuming reverse works correctly for the tail of
     [1;2;3], choose "f" so that reverse works
     correctly for [1;2;3].
     This is called induction.
   *)

  (* Assume rule: 
     reverse [2;3] = [3;2]
   *)

    reverse [1;2;3]
  = fold_right f [1;2;3] []   (* by definition of reverse *)
  = f 1 (fold_right f [2;3] [])  (* Cons rule *)
  = f 1 (reverse [2;3])          (* definition of reverse *)
  = f 1 [3;2]                    (* assumption *)
  
    ???

  = [3;2;1]      (* by definition of f *)

(*
  How can we choose f so that

   f 1 [3;2] = [3;2;1]

  is true?
 *)

  let f = (fun e l = l @ [e])
  let reverse l = fold_right (fun e l -> l @ [e]) l []

 (* MORE CELEBRATION *)

(* fold_left *)

(* fold_left is very similar to fold_right,
   except that it folds in elements from left
   to right. It also swaps the order of its arguments.

fold_left takes three arguments:
  1) a function f defining how an element is
     incorporated into a result
  2) an initial result r
  3) a list of elements

  the second argument is the result for the prefix of
  the list processed so far (fold_left processes 
  left to right).

  Rules for fold_left:
  Nil rule:
    fold_left f r []       = r
  Cons rule:
    fold_left f r (hd::tl) = fold_left f (f r hd) tl

  Base case : []
  Recursive case: (hd::tl)

  What changes on the right hand side of the Cons rule?
    - call to f
    - list argument to fold_right gets smaller
      - from (hd::tl) to tl.
    "one call to f per element in the list"

 *)

let rec fold_left f r l =
  match l with
  | []     -> r
  | hd::tl -> fold_left f (f r hd) tl
;;

let _  = fold_left (fun x y -> x + y) 0 [1;2;3];;

(*
  Exercise: 
    Check that the implementation of fold_left 
    obeys the rules.
 *)

(*
Implement reverse again, as a single call to fold_left.

Same as before. Find expressions for "f" and "r" so that

 *)

let reverse l = fold_left f r l

(*
    reverse []
  = fold_left f r []    (* definition of reverse *)
  = r                   (* Nil rule *)
  = []
 *)

(* 
let's use this rule for f itself:
  
For any list "prefix" and element "last", 
  f (fold_left f r prefix) last 
= fold_left f r (prefix @ [last])

   Examples:
     f (reverse []) 1 = reverse [1]
     f [] 1 = [1]

     f (reverse [1]) 2 = reverse [1;2]
     f [1] 2 = [2;1]

     f (reverse [1;2]) 3 = reverse [1;2;3]
     f [2;1] 3 = [3;2;1]

   Define f so that:
     f [] 1 = [1]
     f [1] 2 = [2;1]
     f [2;1] 3 = [3;2;1]

  let f = fun rev_prefix last -> last :: rev_prefix
 *)

let reverse l = 
  fold_left (fun prefix last -> last :: prefix) [] l

let _ = reverse [1;2;3];;

(* CELEBRATION *)

(* Variables and scope *)

(*
 The key question is to know for each 
 variable *reference*, which
 variable *definition* is *in scope*.
 *)

let x = 3
let double n = n * 2
let double = fun n -> (n*2)
let _ = double x;;
let _ = n;;

(*
OCaml keeps track of an *environment* in which to 
evaluate each expression it encounters, mapping
*names* in scope to their *values*.

Notation:

  []      - empty environment
  [x=32, y = "foo"] 
     - evironment containing "x" and "y"
 *)

(*
expression             environment
----------------------------------------------------------------
let y=4 in y*2         []
y*2                    [y = 4]
4*2                    [y = 4]
8                      [y = 4]
8                      []

Evaluation of let:
 - Add the binding y = 4 to the environment.
 - evaluate the "body" of the let expression.
 - once we reach a value, revert to the previous environment.

In general:
  Variable reference refers to the definition in the *nearest enclosing
  scope TEXTUALLY*.
  
  - earlier definitions are *shadowed* within the nested scope

  let x = 34 in 
     let y = x + 1 in
       let x = x + 32 in             
           x + y;;

  Which binding of x does (x + 1) refer to?    x = 34
  Which binding of x does (x + 32) refer to?   x = 34
  Which binding of x does (x + y) refer to?    x = x + 32


This is called *static* or *lexical* scoping: You can tell at compile
time what variable definition is referred to by each variable use --
the nearest enclosing definition textually.


  let x = 34 in 
     let y = x + 1 in
       let x = x + 32 in             []
           x + y;;

    -- bind x and evaluate the body

  let y = x + 1 in
    let x = x + 32 in                [x = 34]
        x + y;;

    -- lookup x

  let y = 34 + 1 in
    let x = x + 32 in                [x = 34]
        x + y;;

    -- evaluate

  let y = 35 in
    let x = x + 32 in                [x = 34]
        x + y;;

    -- bind y, evaluate body

  let x = x + 32 in                  [y = 35, x = 34]
      x + y;;

    -- lookup x 

  let x = 34 + 32 in                 [y = 35, x = 34]
      x + y;;

    -- evaluate 

  let x = 66 in                      [y = 35, x = 34]
      x + y;;

    -- bind x, evaluate body

  x + y                              [x = 66, y = 35, x = 34]

    -- lookup x

  66 + y                             [x = 66, y = 35, x = 34]

    -- lookup y

  66 + 35                            [x = 66, y = 35, x = 34]

    -- evaluate

  101                                [x = 66, y = 35, x = 34]

    -- evaluated body of inner let x, revert 

  101                                [y = 35, x = 34]

    -- evaluated body of let y, revert 

  101                                [x = 35]

    -- evaluated body of outer let x, revert 

  101                                []

 *)


let x = 3;;

  let x = 34 in 
     let y = x + 1 in
       let x = x + 32 in            
           x + y;;

let _ = x;;
let _ = y;;


(*

Top-level bindings
# let x = 5;;

Are in scope for the rest of the program. 
Stored in "the top-level environment".

 *)

let x = 45;;
let f y = x + y;;
let _ = f 3;;
let x = 12;;
let _ = x;;
let _ = f 3;;

(*
What is the result of the second invocation of f?
  - *shadowing*, not update
  - this is a functional language!

Note: the first x is shadowed when f is invoked,
but it is still the one used!

 *)

(*

Initial Top-level environment: []

# let x = 45

Top-level environment: [x = 45]

# let f y = x + y

  - f evaluates to the function (fun y -> x + y)
  - x refers to the current environment [x = 45]
  - We attach the environment to the function so we 
    can look up x later.
  - Notation:  (fun y -> x + y)[x = 45]

Top-level environment: 
  [f = (fun y -> x + y)[x = 45], x = 45]

# f 3        

  - Extend f's environment with y = 3, and run body.

  x + y      [y = 3, x = 45]
  45 + y     [y = 3, x = 45]
  45 + 3     [y = 3, x = 45]
  48         [y = 3, x = 45]

  - top-level env is unchanged.

Top-level environment: 
  [f = (fun y -> x + y)[x = 45], x = 45]

# let x = 12
  
Top-level environment: 
  [x = 12, f = (fun y -> x + y)[x = 45], x = 45]

# x
  - Use innermost definition of x (x = 12).

Top-level environment: 
  [x = 12, f = (fun y -> x + y)[x = 45], x = 45]

# f 3

  - Extend f's environment with y = 3, and run body.

  x + y      [y = 3, x = 45]
  45 + y     [y = 3, x = 45]
  45 + 3     [y = 3, x = 45]
  48         [y = 3, x = 45]

Final Top-level environment: 
  [x = 12, f = (fun y -> x + y)[x = 45], x = 45]

 *)


let z = 3;;
let x = 3;;
let f y = x + y;;
let z = 4;;
let _ = f z;;

(*

Functional arguments are evaluated before the call
in the current env (not the functions's env).

 *)

