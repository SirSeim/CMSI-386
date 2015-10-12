
(* A simple test harness for the MOCaml REPL. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result, both expressed as strings.
   use the string "dynamic type error" if a DynamicTypeError is expected to be raised.
   use the string "match failure" if a MatchFailure is expected to be raised.
   use the string "implement me" if an ImplementMe exception is expected to be raised

   call the function runReplTests() to run these tests
*)
let replTests = [   
    ("3",                       "val - = 3")
  ; ("false",                   "val - = false")
  ; ("let x = 34",              "val x = 34")
  ; ("y",                       "dynamic type error")
  ; ("x + 4",                   "val - = 38")
  ; ( "let rec fact = "
    ^ "fun n -> "
    ^ "match n with "
    ^ "  0 -> 1 "
    ^ "| _ -> n * fact (n-1)",  "val fact = <fun>")
  ; ("fact 5",                  "val - = 120")
  ]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let env' = evalDecl decl env in
  let str = string_of_env env' in
  (str, Env.combine_envs env env')
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error  -> ("parse error",env)
	    | DynamicTypeError msg -> ("dynamic type error", env)
	    | MatchFailure         -> ("match failure", env)
	    | ImplementMe s        -> ("implement me: " ^ s, env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runReplTests() = test replTests;;
  
