(* You don't need to understand or modify anything in this file *)

(* PRINTING ASTs for #ast *)
let string_of_moop (o : moop) : string =
  match o with
  | Plus  -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Eq    -> "Eq"
  | Gt    -> "Gt"
  | Cons  -> "Cons"


let rec string_of_mopat (p : mopat) : string =
  match p with
  | IntPat i -> "IntPat " ^ string_of_int i
  | BoolPat b -> "BoolPat " ^ string_of_bool b
  | WildcardPat -> "WildcardPat"
  | VarPat x -> "VarPat \"" ^ x ^ "\""
  | NilPat -> "NilPat"
  | ConsPat (p1,p2) -> "ConsPat(" ^ string_of_mopat p1 ^ ", " ^ string_of_mopat p2 ^ ")"

let rec string_of_moexpr (e : moexpr) : string =
 let go = string_of_moexpr in
 let go_op = string_of_moop in
 let go_pat = string_of_mopat in
 match e with
 | IntConst i       -> "IntConst " ^ string_of_int i
 | BoolConst b      -> "BoolConst " ^ string_of_bool b
 | Nil              -> "Nil"
 | Var s            -> "Var \"" ^ s ^ "\""
 | BinOp(l,o,r)     -> "BinOp(" ^ go l ^ ", " ^ go_op o ^ ", " ^ go r ^ ")"
 | Negate e         -> "Negate(" ^ go e ^ ")"
 | If(e1,e2,e3)     -> "If(" ^ go e1 ^ ", " ^ go e2 ^ ", " ^ go e3 ^ ")"
 | Fun(p,e)         -> "Fun(" ^ go_pat p ^ ", " ^ go e ^ ")"
 | FunCall(e1,e2)   -> "FunCall(" ^ go e1 ^ ", " ^ go e2 ^ ")"
 | Match(e,cs) -> 
    let pp_cs = List.map (fun (p,e) -> "(" ^ go_pat p ^ ", " ^ go e ^ ")") cs in
    let rec sep (l : string list) : string = 
      match l with
	[] -> ""
      | [s] -> s
      |	hd::tl -> hd ^ "; " ^ sep tl
    in "Match(" ^ go e ^ ", [" ^ sep pp_cs ^ "])"
  | Let(p,e1,e2)    -> "Let(" ^ go_pat p ^ ", " ^ go e1 ^ ", " ^ go e2 ^ ")"
  | LetRec(s,e1,e2) -> "LetRec(" ^ s ^ ", " ^ go e1 ^ ", " ^ go e2 ^ ")"

(* PRETTY PRINTING *)

let rec print_val (v:movalue) :string =
  match v with
      IntVal(i) -> string_of_int i
    | BoolVal(b) -> string_of_bool b
    | FunVal(_) -> "<fun>"
    | NilVal -> "[]"
    | ConsVal(v,lv') -> (print_val v) ^ "::" ^ (print_val lv')

let print_result ((nopt,v):moresult) :string =
  let vstr = print_val v in
  match nopt with
      None -> vstr
    | Some x -> "val " ^ x ^ " = " ^ vstr

let string_of_env (env:moenv) : string =
  let rec unlines ls =
    match ls with
      [] -> ""
    | [l] -> l
    | l::ls -> l ^ "\n" ^ unlines ls
  in
  unlines (List.map (fun (nm,v) -> "val " ^ nm ^ " = " ^ print_val v) (Env.to_list env))

(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moenv =
  match d with
      ShowEnv              -> print_string (string_of_env env); Env.empty_env()
    | ShowAst e            -> print_string (string_of_moexpr e); Env.empty_env()
    | Expr(e)              -> [("-", evalExpr e env)]
    | LetDecl(VarPat x,e)  -> [(x, evalExpr e env)]
    | LetDecl(p, e)        -> patMatch p (evalExpr e env)
    | LetRecDecl (nm,def)  -> Env.add_binding nm (evalExpr (LetRec (nm,def,Var nm)) env)
                                (Env.empty_env ())
      
(* ENTRY POINT *)	  


let readEvalPrint(env:moenv) :moenv =
  let _ = print_string "mocaml# "; flush stdout in
  let lexbuf = Lexing.from_channel stdin in
  let decl = main token lexbuf in
  let env' = evalDecl decl env in
  let msg = string_of_env env' in
  print_string msg;
  print_newline();
  flush stdout;
  Env.combine_envs env env'

let rec repl(env:moenv) =
  try
    repl(readEvalPrint(env))
  with
      Eof ->
	exit 0
    | Parsing.Parse_error ->
	print_string "parse error";
	print_newline(); flush stdout; repl(env)
    | DynamicTypeError msg ->
	print_string ("dynamic type error: " ^ msg);
	print_newline(); flush stdout; repl(env)
    | MatchFailure ->
	print_string "match failure";
	print_newline(); flush stdout; repl(env)
    | ImplementMe s ->
	print_string ("ImplementMe: " ^ s);
	print_newline(); flush stdout; repl(env)

	  
let mocaml() =
  print_newline();
  print_string "**********Welcome to MOCaml!**********";
  print_newline(); flush stdout;
  repl(Env.empty_env())

