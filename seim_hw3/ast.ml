(* The representation of MOCaml patterns.

  p ::= intconst | boolconst | _ | var | [] | p::p
   
*)
type mopat =
    IntPat of int
  | BoolPat of bool
  | WildcardPat
  | VarPat of string
  | NilPat
  | ConsPat of mopat * mopat
;;

(* The representation of MOCaml expressions.

op ::= + | - | * | = | > | ::   

You can assume that the first 5 binary operators above apply only to
integers; raise a DynamicTypeError for other cases.  The last
operator, ::, applies only when the right operand is a list value (the
left operand is unrestricted); raise a DynamicTypeError for other
cases.
   
e ::= intconst | boolconst | [] | var | e1 op e2 | -e | if e1 then e2 else e3
    | fun p -> e | e1 e2
    | let p = e1 in e2
    | let rec p = e1 in e2
    | match e with p1 -> e1 '|' ... '|' pn -> en

MOCaml also supports the syntax [e1;...;en] for creating lists, but it
is converted by the parser into the expression e1::e2::...::en::[].
*)
type moop = Plus | Minus | Times | Eq | Gt | Cons
type moexpr =
    IntConst of int     (* integer literal *)
  | BoolConst of bool   (* boolean literal *)
  | Nil                 (* []              *)
  | Var of string       (* variable name   *)

  (* EXPR + EXPR
     EXPR - EXPR
      ... etc
   *)
  | BinOp of moexpr * moop * moexpr

  (* - EXPR *)
  | Negate of moexpr 

  (* if EXPR then EXPR else EXPR *)
  | If of moexpr * moexpr * moexpr

  (* fun PAT -> EXPR *)
  | Fun of mopat * moexpr

  (* EXPR EXPR *)
  | FunCall of moexpr * moexpr

  (* match EXPR with PAT1 -> EXPR1 | PAT2 -> EXPR2 | ... | PATn -> EXPRn *)
  | Match of moexpr * (mopat * moexpr) list

  (* let PAT = EXPR in EXPR *)
  | Let of mopat * moexpr * moexpr

  (* let rec nm = EXPR in EXPR *)
  | LetRec of string * moexpr * moexpr


(* The representation of MOCaml values, which are the results of
evaluating expressions.

  v ::= intconst | boolconst | fun p -> e | [] | v::v
*)
type movalue =
    IntVal of int
  | BoolVal of bool
      (* A function value carries its lexical environment with it! *)
      (* If the function is recursive it also carries around its own name. *)
  | FunVal of string option * mopat * moexpr * (movalue Env.env)
    (* Lists *)
  | NilVal
  | ConsVal of movalue * movalue


(* The representation of value environments, which map strings to
values.  See env.ml for the definition of the Env.env type and
associated operations. *)
type moenv = movalue Env.env

(* You don't need to understand anything below here.
   These definitions are for the REPL.
 *)

(* The representation of Mocaml declarations.

d ::= e | let x = e | let rec f p = e
   
*)

type modecl =
  | ShowEnv
  | ShowAst of moexpr
  | Expr of moexpr
  | LetDecl of mopat * moexpr
  | LetRecDecl of string * moexpr
   
(* The result from evaluating a declaration -- an optional name that
was declared along with the value of the right-hand-side
expression. *)
type moresult = string option * movalue


