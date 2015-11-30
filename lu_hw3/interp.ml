(* Name: Adrian Lu

   UID: 977089943

   Others With Whom I Discussed Things: Rodrigo "Hot Rod" Seim, Josh Kudoratheexplora, Loryn Konchyn, Matthew "(* Celebrate! *)" Brown

   Other Resources I Consulted: i dun evn kno ne mor
   
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* TESTING *)

(* We need to be able to test code that might throw an exception. In particular,
   we want to test that it does throw a particular exception when we expect it to.
 *)

type 'a or_exception = Value of 'a | Exception of exn

(* General-purpose testing function. You don't need to use this. I'll provide
   specialized test functions you can use instead.
 *)

let tester (nm : string) (thunk : unit -> 'a) (expected : 'a or_exception) =
  let got = try Value (thunk ()) with e -> Exception e in
  let msg = match (expected, got) with
      (e1,e2)              when e1 = e2   -> "OK"
    | (Value _,  Value _)                 -> "FAILED (value error)"
    | (Exception _, Value _)              -> "FAILED (expected exception)"
    | (_, Exception (ImplementMe msg))    -> "FAILED: ImplementMe(" ^ msg ^ ")"
    | (_, Exception (MatchFailure))       -> "FAILED: MatchFailure"
    | (_, Exception (DynamicTypeError s)) -> "FAILED: DynamicTypeError(" ^ s ^ ")"
    | (_, Exception e)                    -> "FAILED: " ^ Printexc.to_string e
  in
  print_string (nm ^ ": " ^ msg ^ "\n");
  flush stdout

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
      (IntPat(i), IntVal(j)) when i = j -> Env.empty_env()
    | (IntPat(_), IntVal(_)) -> raise MatchFailure
    | (IntPat(_), _) -> raise MatchFailure
    | (BoolPat(i), BoolVal(j)) when i = j -> Env.empty_env()
    | (BoolPat(_), BoolVal(_)) -> raise MatchFailure
    | (BoolPat(_), _) -> raise MatchFailure
    | (VarPat(i), _) -> (Env.add_binding (i) (value) (Env.empty_env()))
    | (ConsPat(ia, ib), ConsVal(ja, jb)) -> (Env.combine_envs (patMatch ia ja) (patMatch ib jb))
    | (NilPat, NilVal) -> Env.empty_env()
    | (NilPat, _) -> raise MatchFailure
    | (WildcardPat, _) -> Env.empty_env()
    | _ -> raise MatchFailure


(* patMatchTest defines a test case for the patMatch function.
   inputs: 
     - nm: a name for the test, for the status report.
     - pat: a mini-OCaml pattern, the first input to patMatch
     - value: a mini-OCaml value, the second input to patMatch
     - expected: the expected result of running patMatch on these inputs.
 *)

let patMatchTest (nm,pat,value,expected) =
  tester ("patMatch:" ^ nm) (fun () -> patMatch pat value) expected

(* Tests for patMatch function. 
      ADD YOUR OWN! 
 *)
let patMatchTests = [
    (* integer literal pattern tests *)
    ("IntPat/1", IntPat 5, IntVal 5,      Value [])
  ; ("IntPat/2", IntPat 5, IntVal 6,      Exception MatchFailure)
  ; ("IntPat/3", IntPat 5, BoolVal false, Exception MatchFailure)

    (* boolean literal pattern tests *)   
  ; ("BoolPat/1", BoolPat true, BoolVal true,  Value [])
  ; ("BoolPat/2", BoolPat true, BoolVal false, Exception MatchFailure)
  ; ("BoolPat/3", BoolPat true, IntVal 0,      Exception MatchFailure)

    (* wildcard pattern *)
  ; ("WildcardPat/1", WildcardPat, IntVal 5,     Value [])
  ; ("WildcardPat/2", WildcardPat, BoolVal true, Value [])

    (* variable pattern *)
  ; ("VarPat/1", VarPat "x", IntVal 5,     Value [("x", IntVal 5)])
  ; ("VarPat/2", VarPat "y", BoolVal true, Value [("y", BoolVal true)])

    (* Nil pattern *)
  ; ("NilPat/1", NilPat, NilVal,       Value [])
  ; ("NilPat/2", NilPat, IntVal 5,     Exception MatchFailure)
  ; ("NilPat/3", NilPat, BoolVal true, Exception MatchFailure)

    (* cons pattern *)
  ; ("ConsPat/1", ConsPat(IntPat 5, NilPat), ConsVal(IntVal 5, NilVal), 
     Value [])
  ; ("ConsPat/2", ConsPat(IntPat 5, NilPat), ConsVal(BoolVal true, NilVal), 
     Exception MatchFailure)
  ; ("ConsPat/3", ConsPat(VarPat "hd", VarPat "tl"), ConsVal(IntVal 5, NilVal), 
     Value [("tl", NilVal); ("hd", IntVal 5)])
  ]
;;

(* Run all the tests *)
List.map patMatchTest patMatchTests;;

(* To evaluate a match expression, we need to choose which case to take.
   Here, match cases are represented by a pair of type (mopat * moexpr) --
   a pattern and the expression to be evaluated if the match succeeds.
   Try matching the input value with each pattern in the list. Return
   the environment produced by the first successful match (if any) along
   with the corresponding expression. If there is no successful match,
   raise the MatchFailure exception.
 *)
let rec matchCases (value : movalue) (cases : (mopat * moexpr) list) : moenv * moexpr =
  match (value, cases) with
    (value, (pattern, expression)::tl) -> (try ((patMatch pattern value), expression) with
      | MatchFailure -> matchCases value tl)
  | _ -> raise MatchFailure
(* We'll use these cases for our tests.
   To make it easy to identify which case is selected, we make
 *)
let testCases : (mopat * moexpr) list =
  [(IntPat 1, Var "case 1");
   (IntPat 2, Var "case 2");
   (ConsPat (VarPat "head", VarPat "tail"), Var "case 3");
   (BoolPat true, Var "case 4")
  ]

(* matchCasesTest: defines a test for the matchCases function.
   inputs:
     - nm: a name for the test, for the status report.
     - value: a mini-OCaml value, the first input to matchCases
     - expected: the expected result of running (matchCases value testCases).
 *)
let matchCasesTest (nm, value, expected) =
  tester ("matchCases:" ^ nm) (fun () -> matchCases value testCases) expected

(* Tests for matchCases function. 
      ADD YOUR OWN! 
 *)
let matchCasesTests = [
    ("IntVal/1", IntVal 1, Value ([], Var "case 1"))
  ; ("IntVal/2", IntVal 2, Value ([], Var "case 2"))

  ; ("ConsVal", ConsVal(IntVal 1, ConsVal(IntVal 2, NilVal)), 
     Value ([("tail", ConsVal(IntVal 2, NilVal)); ("head", IntVal 1)], Var "case 3"))

  ; ("BoolVal/true",  BoolVal true,  Value ([], Var "case 4"))
  ; ("BoolVal/false", BoolVal false, Exception MatchFailure)
  ]
;;

List.map matchCasesTest matchCasesTests;;

(* "Tying the knot".
   Make a function value recursive by setting its name component.
 *)
let tieTheKnot nm v =
  match v with
  | FunVal(None,pat,def,env) -> FunVal(Some nm,pat,def,env)
  | _                        -> raise (DynamicTypeError "tieTheKnot expected a function")

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
    (* an integer constant evaluates to itself *)
    IntConst(i) -> IntVal(i)
  | BoolConst(i) -> BoolVal(i)
  | Nil -> NilVal
  | Var(i) -> (try (Env.lookup i env) with
      Env.NotBound -> raise (DynamicTypeError "varibale not found in environment")
    )
  | BinOp(i, Plus, j) -> (match (evalExpr i env) with
      | IntVal(ival) -> (match (evalExpr j env) with
          | IntVal(jval) -> IntVal(ival + jval)
          | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
        )
      | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
    )
  | BinOp(i, Minus, j) -> (match (evalExpr i env) with
      | IntVal(ival) -> (match (evalExpr j env) with
          | IntVal(jval) -> IntVal(ival - jval)
          | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
        )
      | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
    )
  | BinOp(i, Times, j) -> (match (evalExpr i env) with
      | IntVal(ival) -> (match (evalExpr j env) with
          | IntVal(jval) -> IntVal(ival * jval)
          | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
        )
      | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
    )
  | BinOp(i, Eq, j) -> (match (evalExpr i env) with
      | IntVal(ival) -> (match (evalExpr j env) with
          | IntVal(jval) -> BoolVal(ival = jval)
          | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
        )
      | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
    )
  | BinOp(i, Gt, j) -> (match (evalExpr i env) with
      | IntVal(ival) -> (match (evalExpr j env) with
          | IntVal(jval) -> BoolVal(ival > jval)
          | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
        )
      | _ -> raise (DynamicTypeError "I can only perform this action on Integers")
    )
  | BinOp(i, Cons, j) -> ConsVal(evalExpr i env, evalExpr j env)
  | Negate(BinOp(i, Cons, j)) -> ConsVal(evalExpr (Negate(i)) env, evalExpr (Negate(j)) env)
  | Negate(i) -> (match (evalExpr i env) with
      | IntVal(ival) -> IntVal(-ival)
      | BoolVal(ival) -> BoolVal(not ival)
      | NilVal -> NilVal
      | _ -> raise MatchFailure
    )
  | Negate(BinOp(i, Cons, j)) -> ConsVal(evalExpr (Negate(i)) env, evalExpr (Negate(j)) env)
  | If(i, j, k) -> if ((evalExpr i env) = BoolVal(true)) then evalExpr j env else evalExpr k env
  | Fun(x, y) -> FunVal(None, x, y, env)
  | FunCall(a, b) -> (match (evalExpr a env) with
      | FunVal(n, p, e, v) -> let newEnv = Env.combine_envs env (patMatch p (evalExpr b v)) in evalExpr e newEnv
      | _ -> raise (DynamicTypeError "Input a proper FunVal")
    )
  | Match(ex, l) -> let v, e = matchCases (evalExpr ex env) l in evalExpr e env
  | Let(p, e1, e2) -> evalExpr e2 (Env.combine_envs env (patMatch p (evalExpr e1 env)))
  | LetRec(nm, e1, e2) -> tieTheKnot nm (evalExpr (FunCall (e1,e2)) env)
  | _ -> raise MatchFailure

(* evalExprTest defines a test case for the evalExpr function.
   inputs: 
     - nm: a name for the test, for the status report.
     - expr: a mini-OCaml expression to be evaluated
     - expected: the expected result of running (evalExpr expr [])
                 (either a value or an exception)
 *)
let evalExprTest (nm,expr,expected) = 
  tester ("evalExpr:" ^ nm) (fun () -> evalExpr expr []) expected

(* Tests for evalExpr function. 
      ADD YOUR OWN!
 *)
let evalExprTests = [
    ("IntConst",    IntConst 5,                                     Value (IntVal 5))
  ; ("BoolConst",   BoolConst true,                                 Value (BoolVal true))
  ; ("Nil",         Nil,                                            Value (NilVal))
  ; ("Plus",        BinOp(IntConst 1, Plus, IntConst 1),            Value (IntVal 2))
  ; ("BadPlus",     BinOp(BoolConst true, Plus, IntConst 1),        Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("Minus",       BinOp(IntConst 2, Minus, IntConst 1),           Value (IntVal 1))
  ; ("BadMinus",    BinOp(IntConst 2, Minus, BoolConst true),       Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("Times",       BinOp(IntConst 2, Times, IntConst 2),           Value (IntVal 4))
  ; ("BadTimes",    BinOp(BoolConst true, Times, IntConst 4),       Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("EqualsTrue",  BinOp(IntConst 2, Eq, IntConst 2),              Value (BoolVal true))
  ; ("EqualsFalse", BinOp(IntConst 2, Eq, IntConst 3),              Value (BoolVal false))
  ; ("BadEquals",   BinOp(IntConst 2, Eq, BoolConst true),          Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("GtTrue",     BinOp(IntConst 420, Gt, IntConst 69),              Value (BoolVal true))
  ; ("GtFalse",    BinOp(IntConst 69, Gt, IntConst 420),              Value (BoolVal false))
  ; ("BadGt",       BinOp(BoolConst true, Gt, IntConst 3),          Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("NegInt",      Negate(IntConst 420),                             Value (IntVal (-420)))
  ; ("NegBool",     Negate(BoolConst true),                         Value (BoolVal false))
  ; ("NegNil",      Negate(Nil),                                    Value (NilVal))
  ; ("NegSum",      Negate(BinOp(IntConst 69, Plus, IntConst 420)),    Value (IntVal (-489)))
  ; ("BadNeg",      Negate(BinOp(BoolConst true, Plus, IntConst 1)),Exception (DynamicTypeError "I can only perform this action on Integers"))
  ; ("If",          If(BoolConst true, IntConst 4, IntConst 2),     Value (IntVal 4))
  ; ("BetterIf",    If(BinOp(IntConst 2, Eq, IntConst 2),
      IntConst 6, IntConst 3),                                      Value (IntVal 6))

  ; ("Let",         Let(VarPat "x", IntConst 1, Var "x"),       Value (IntVal 1))
  ; ("Fun",         FunCall(
      Fun(VarPat "x", Var "x"),
      IntConst 5),                         Value (IntVal 5))
  ]
;;

List.map evalExprTest evalExprTests;;

(* See test.ml for a nicer way to write more tests! *)