(* Name:

   UID:

   Others With Whom I Discussed Things:

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
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | _ -> raise (ImplementMe "patMatch")


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
  raise (ImplementMe "matchCases")

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
  | _ -> raise (ImplementMe "evalExpr")

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
    ("IntConst",    IntConst 5,                              Value (IntVal 5))
  ; ("BoolConst",   BoolConst true,                          Value (BoolVal true))
  ; ("Plus",        BinOp(IntConst 1, Plus, IntConst 1),     Value (IntVal 2))
  ; ("BadPlus",     BinOp(BoolConst true, Plus, IntConst 1), Exception (DynamicTypeError "You should change this to be a more informative message"))
  ; ("Let",         Let(VarPat "x", IntConst 1, Var "x"),    Value (IntVal 1))
  ; ("Fun",         FunCall(
			Fun(VarPat "x", Var "x"),
			IntConst 5),                         Value (IntVal 5))
  ]
;;

List.map evalExprTest evalExprTests;;

(* See test.ml for a nicer way to write more tests! *)
  

