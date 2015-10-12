(* Name: Lauren Konchan

   UID: 975931225

   Others With Whom I Discussed Things: Josh, Ed

   Other Resources I Consulted: 
   
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
    | (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | (BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
    | (WildcardPat, _ ) -> Env.empty_env()
    | (VarPat(i), _) -> Env.add_binding (i) (value) (Env.empty_env())
    | (NilPat, NilVal) -> Env.empty_env()
    | (ConsPat(i, o), ConsVal(j, k)) -> Env.combine_envs (patMatch i j) (patMatch o k)
    |  _ -> raise MatchFailure


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
  ; ("ConsPat/1", ConsPat(IntPat 5, NilPat), ConsVal(IntVal 5, NilVal),    Value [])
  ; ("ConsPat/2", ConsPat(IntPat 5, NilPat), ConsVal(BoolVal true, NilVal),      Exception MatchFailure)
  ; ("ConsPat/3", ConsPat(VarPat "hd", VarPat "tl"), ConsVal(IntVal 5, NilVal),      Value [("tl", NilVal); ("hd", IntVal 5)])
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
   (v, (x,y)::tl ) -> (try (patMatch x v, y) with MatchFailure -> matchCases v tl)
  | _ -> raise MatchFailure

(* We'll use these cases for our tests.
   To make it easy to identify which case is selected, we make
 *)
let testCases : (mopat * moexpr) list =
  [(IntPat 1, Var "case 1");
   (IntPat 2, Var "case 2");
   (ConsPat (VarPat "head", VarPat "tail"), Var "case 3");
   (BoolPat true, Var "case 4");
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
  | IntConst(i) -> IntVal(i)
  | BoolConst(b) -> BoolVal(b)
  | Nil -> NilVal
  | Var(s) -> Env.lookup s env

  | BinOp(IntConst(x), Plus, IntConst(y)) -> IntVal(x + y)
  | BinOp(_, Plus, _) -> raise (DynamicTypeError "Can only add integers together.")

  | BinOp(IntConst(x), Minus, IntConst(y)) -> IntVal(x - y)
  | BinOp(_, Minus, _) -> raise (DynamicTypeError "Can only subtract integers from eachother.")

  | BinOp(IntConst(x), Times, IntConst(y)) -> IntVal(x * y)
  | BinOp(_, Times, _) -> raise (DynamicTypeError "Can only multiply integers together.")

  | BinOp(IntConst(x), Eq, IntConst(y)) -> BoolVal(x = y)
  | BinOp(_, Eq, _) -> raise (DynamicTypeError "Can only compare integers together.")

  | BinOp(IntConst(x), Gt, IntConst(y)) -> BoolVal(x > y)
  | BinOp(_, Gt, _) -> raise (DynamicTypeError "Can only compare integers together.")

  | BinOp(head, Cons, tail) -> ConsVal(evalExpr head env, evalExpr tail env)

  | Negate(BoolConst(k)) -> if k = true then BoolVal(false) else BoolVal(true)
  | Negate(IntConst(x)) -> IntVal(0 - x)
  | Negate(_) -> raise (DynamicTypeError "Only negates negatable things.")

  | If(e1, e2, e3) -> if (evalExpr e1 env) = BoolVal(true) then evalExpr e2 env else evalExpr e3 env

  | Fun(pattern, expression) -> FunVal(None, pattern, expression, env)

  | FunCall(e1, e2) -> match (evalExpr e1 env) with 
                        FunVal(n,p,e,v) -> let environment = Env.combine_envs env (patMatch p (evalExpr e2 v)) in evalExpr e environment
                        | _ -> raise (DynamicTypeError "Requires a FunVal")

  | Match(e1, (pattern, e2), tail) -> match (evalEx e1 env) with
                                        | pattern
  (*| Let(pattern, e1, e2) -> evalExpr e1 env 
  | LetRec(Strng, e1, e2) ->*)
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
    ("IntConst",    IntConst 5,                                   Value (IntVal 5))
  ; ("BoolConst",   BoolConst true,                               Value (BoolVal true))
  ; ("Nil",         Nil,                                          Value NilVal)

    (*BIN OPS*)
  ; ("Plus",        BinOp(IntConst 1, Plus, IntConst 1),          Value (IntVal 2))
  ; ("BadPlus",     BinOp(BoolConst true, Plus, IntConst 1),      Exception (DynamicTypeError "Can only add integers together."))
  ; ("Minus",       BinOp(IntConst 5, Minus, IntConst 1),         Value (IntVal 4))
  ; ("BadMinus",    BinOp(IntConst 5, Minus, BoolConst true),     Exception (DynamicTypeError "Can only subtract integers from eachother."))
  ; ("Multiply",    BinOp(IntConst 2, Times, IntConst 10),        Value (IntVal 20))
  ; ("BadMult",     BinOp(BoolConst false, Times, BoolConst true),Exception (DynamicTypeError "Can only multiply integers together."))
  ; ("Equals",      BinOp(IntConst 72, Eq, IntConst 72),          Value (BoolVal true))
  ; ("BadEquals",   BinOp(BoolConst false, Eq, IntConst 72),      Exception (DynamicTypeError "Can only compare integers together."))
  ; ("GreaterThan", BinOp(IntConst 3, Gt, IntConst 72),           Value (BoolVal false))
  ; ("BadGT",       BinOp(BoolConst false, Gt, IntConst 3),       Exception (DynamicTypeError "Can only compare integers together."))
  ; ("Cons",        BinOp(IntConst 5, Cons, IntConst 6),          Value (ConsVal (IntVal 5, IntVal 6)))

    (*NEGATE BIN OPS*)
  ; ("NegateBool",  Negate(BoolConst true),                       Value (BoolVal false))
  ; ("NegateInt",   Negate(IntConst 5),                           Value (IntVal (-5)))
(*; ("NegatePlus",  Negate(BinOp(IntConst 1, Plus, IntConst 1)),  Value (IntVal 0))
  ; ("NegateSub",   Negate(BinOp(IntConst 5, Minus, IntConst 1)), Value (IntVal 6))
  ; ("NegateTimes", Negate(BinOp(IntConst 10, Times, IntConst 2)),Value (IntVal 5))
  ; ("NegateEQ",    Negate(BinOp(IntConst 72, Eq, IntConst 72)),  Value (BoolVal false))
  ; ("NegateGT",    Negate(BinOp(IntConst 72, Gt, IntConst 80)),  Value (BoolVal true))
  ; ("BadNegate",   Negate(Nil),                                  Exception (DynamicTypeError "Only negates negatable things."))
*)
    (*IF*)
  ; ("Ifone",      If(BinOp(IntConst 5, Gt, IntConst 6), BoolConst true, BoolConst false),Value (BoolVal false))  
  ; ("Iftwo",      If(Negate(BoolConst false), IntConst 72, IntConst 3),                  Value (IntVal 72))

    (*FUN/call*)
  ; ("Fun",         Fun(VarPat "x", IntConst 72),                 Value (FunVal (None, VarPat("x"), IntConst(72), [])))
  ; ("FunCall",     FunCall(Fun(VarPat "x", Var "x"),IntConst 5), Value (IntVal 5))
  ; ("Let",         Let(VarPat "x", IntConst 1, Var "x"),         Value (IntVal 1));
  ]
;;

List.map evalExprTest evalExprTests;;

(* See test.ml for a nicer way to write more tests! *)
  

