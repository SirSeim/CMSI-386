type token =
  | INT of (int)
  | BOOL of (bool)
  | VAR of (string)
  | FUNCTION
  | PIPE
  | WILDCARD
  | FN_ARROW
  | IF
  | THEN
  | ELSE
  | LET
  | REC
  | EQ
  | IN
  | MATCH
  | WITH
  | LBRACK
  | RBRACK
  | CONS
  | SEMICOLON
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | TIMES
  | GT
  | COLON
  | EOD
  | SHOW_ENV
  | SHOW_AST

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
# 41 "parser.ml"
let yytransl_const = [|
  260 (* FUNCTION *);
  261 (* PIPE *);
  262 (* WILDCARD *);
  263 (* FN_ARROW *);
  264 (* IF *);
  265 (* THEN *);
  266 (* ELSE *);
  267 (* LET *);
  268 (* REC *);
  269 (* EQ *);
  270 (* IN *);
  271 (* MATCH *);
  272 (* WITH *);
  273 (* LBRACK *);
  274 (* RBRACK *);
  275 (* CONS *);
  276 (* SEMICOLON *);
  277 (* LPAREN *);
  278 (* RPAREN *);
  279 (* PLUS *);
  280 (* MINUS *);
  281 (* TIMES *);
  282 (* GT *);
  283 (* COLON *);
  284 (* EOD *);
  285 (* SHOW_ENV *);
  286 (* SHOW_AST *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\005\000\005\000\007\000\
\007\000\008\000\008\000\008\000\009\000\009\000\010\000\010\000\
\011\000\011\000\012\000\012\000\012\000\012\000\013\000\013\000\
\013\000\014\000\014\000\006\000\006\000\015\000\015\000\004\000\
\004\000\016\000\016\000\016\000\016\000\016\000\000\000"

let yylen = "\002\000\
\001\000\002\000\003\000\002\000\005\000\006\000\001\000\004\000\
\006\000\004\000\006\000\007\000\001\000\003\000\003\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\001\000\002\000\
\001\000\002\000\001\000\001\000\003\000\003\000\001\000\001\000\
\002\000\001\000\003\000\003\000\004\000\004\000\005\000\001\000\
\003\000\001\000\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\031\000\032\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\001\000\
\000\000\000\000\013\000\000\000\000\000\021\000\000\000\025\000\
\027\000\045\000\044\000\000\000\000\000\000\000\000\000\042\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\000\000\000\000\000\000\002\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\030\000\003\000\014\000\015\000\017\000\000\000\000\000\
\000\000\046\000\010\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\035\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\009\000\000\000\006\000\011\000\
\000\000\012\000\000\000\037\000\000\000\000\000\000\000\039\000"

let yydgoto = "\002\000\
\015\000\016\000\040\000\031\000\018\000\083\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\041\000\100\000\033\000"

let yysindex = "\011\000\
\017\255\000\000\000\000\000\000\000\000\053\000\061\255\028\000\
\061\255\224\255\061\255\235\255\242\254\061\255\000\000\000\000\
\250\254\247\254\000\000\248\254\008\255\000\000\235\255\000\000\
\000\000\000\000\000\000\019\255\053\000\042\255\037\255\000\000\
\029\255\036\000\041\255\048\255\040\255\054\255\000\000\036\255\
\053\255\051\255\235\255\000\000\046\255\000\000\061\000\061\000\
\061\000\061\000\061\000\235\255\000\000\058\255\000\000\061\255\
\053\000\072\255\068\255\061\255\070\255\061\255\053\000\061\255\
\000\000\000\000\000\000\000\000\000\000\000\000\008\255\008\255\
\235\255\000\000\000\000\000\000\074\255\061\255\079\255\061\255\
\251\254\084\255\000\000\000\000\061\255\078\255\061\255\021\255\
\061\255\000\000\061\255\083\255\000\000\061\255\000\000\000\000\
\093\255\000\000\053\000\000\000\095\255\061\255\093\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\241\255\156\255\000\000\090\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\255\000\000\000\000\000\000\000\000\000\000\000\000\087\255\
\000\000\000\000\112\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\176\255\196\255\
\134\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\070\000\000\000\000\000\000\000\000\000\000\000\085\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\255\255\253\255\000\000\000\000\019\000\000\000\
\245\255\000\000\246\255\240\255\251\255\043\000\008\000\000\000"

let yytablesize = 369
let yytable = "\017\000\
\032\000\043\000\032\000\047\000\037\000\035\000\053\000\038\000\
\089\000\042\000\049\000\001\000\045\000\044\000\050\000\051\000\
\048\000\003\000\004\000\005\000\006\000\046\000\090\000\032\000\
\007\000\054\000\053\000\008\000\032\000\040\000\059\000\009\000\
\052\000\010\000\094\000\040\000\039\000\011\000\071\000\072\000\
\012\000\073\000\055\000\056\000\040\000\013\000\014\000\057\000\
\095\000\060\000\061\000\032\000\062\000\076\000\075\000\064\000\
\053\000\032\000\079\000\082\000\081\000\003\000\004\000\005\000\
\006\000\068\000\069\000\070\000\007\000\063\000\065\000\034\000\
\066\000\067\000\077\000\009\000\086\000\010\000\088\000\074\000\
\078\000\011\000\080\000\092\000\012\000\093\000\085\000\096\000\
\087\000\097\000\091\000\089\000\098\000\032\000\023\000\101\000\
\094\000\099\000\023\000\023\000\103\000\102\000\023\000\023\000\
\034\000\023\000\084\000\023\000\023\000\023\000\104\000\023\000\
\023\000\023\000\023\000\023\000\024\000\023\000\000\000\000\000\
\024\000\024\000\000\000\000\000\024\000\024\000\000\000\024\000\
\000\000\024\000\024\000\024\000\000\000\024\000\024\000\024\000\
\024\000\024\000\022\000\024\000\000\000\000\000\022\000\022\000\
\000\000\000\000\022\000\022\000\000\000\022\000\000\000\022\000\
\022\000\022\000\000\000\022\000\022\000\022\000\022\000\022\000\
\018\000\022\000\000\000\000\000\018\000\018\000\000\000\000\000\
\018\000\018\000\000\000\018\000\000\000\018\000\018\000\018\000\
\000\000\018\000\018\000\018\000\019\000\018\000\000\000\018\000\
\019\000\019\000\000\000\000\000\019\000\019\000\000\000\019\000\
\000\000\019\000\019\000\019\000\000\000\019\000\019\000\019\000\
\020\000\019\000\000\000\019\000\020\000\020\000\000\000\000\000\
\020\000\020\000\000\000\020\000\000\000\020\000\020\000\020\000\
\000\000\020\000\020\000\020\000\000\000\020\000\000\000\020\000\
\003\000\004\000\005\000\006\000\000\000\000\000\000\000\007\000\
\000\000\000\000\034\000\003\000\004\000\005\000\009\000\000\000\
\010\000\039\000\000\000\000\000\011\000\016\000\000\000\012\000\
\000\000\016\000\016\000\010\000\000\000\016\000\016\000\011\000\
\016\000\000\000\016\000\000\000\016\000\000\000\016\000\000\000\
\000\000\007\000\016\000\000\000\016\000\007\000\007\000\000\000\
\000\000\000\000\007\000\000\000\007\000\000\000\007\000\000\000\
\007\000\000\000\007\000\000\000\003\000\004\000\026\000\000\000\
\007\000\027\000\000\000\000\000\003\000\004\000\026\000\036\000\
\000\000\027\000\000\000\000\000\028\000\000\000\000\000\058\000\
\029\000\000\000\000\000\030\000\028\000\003\000\004\000\026\000\
\029\000\000\000\027\000\030\000\000\000\003\000\004\000\005\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\029\000\000\000\000\000\030\000\010\000\036\000\036\000\
\000\000\011\000\000\000\036\000\012\000\036\000\000\000\036\000\
\000\000\036\000\000\000\036\000\000\000\038\000\038\000\000\000\
\000\000\036\000\038\000\000\000\038\000\000\000\038\000\000\000\
\038\000\000\000\038\000\000\000\000\000\000\000\000\000\000\000\
\038\000"

let yycheck = "\001\000\
\006\000\012\000\008\000\013\001\008\000\007\000\023\000\009\000\
\014\001\011\000\019\001\001\000\014\000\028\001\023\001\024\001\
\026\001\001\001\002\001\003\001\004\001\028\001\028\001\029\000\
\008\001\029\000\043\000\011\001\034\000\007\001\034\000\015\001\
\025\001\017\001\014\001\013\001\018\001\021\001\050\000\051\000\
\024\001\052\000\001\001\007\001\022\001\029\001\030\001\019\001\
\028\001\009\001\003\001\057\000\013\001\057\000\056\000\020\001\
\073\000\063\000\060\000\063\000\062\000\001\001\002\001\003\001\
\004\001\047\000\048\000\049\000\008\001\016\001\018\001\011\001\
\022\001\028\001\003\001\015\001\078\000\017\001\080\000\022\001\
\013\001\021\001\013\001\085\000\024\001\087\000\013\001\089\000\
\010\001\091\000\007\001\014\001\094\000\099\000\005\001\099\000\
\014\001\005\001\009\001\010\001\102\000\007\001\013\001\014\001\
\018\001\016\001\064\000\018\001\019\001\020\001\103\000\022\001\
\023\001\024\001\025\001\026\001\005\001\028\001\255\255\255\255\
\009\001\010\001\255\255\255\255\013\001\014\001\255\255\016\001\
\255\255\018\001\019\001\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\005\001\028\001\255\255\255\255\009\001\010\001\
\255\255\255\255\013\001\014\001\255\255\016\001\255\255\018\001\
\019\001\020\001\255\255\022\001\023\001\024\001\025\001\026\001\
\005\001\028\001\255\255\255\255\009\001\010\001\255\255\255\255\
\013\001\014\001\255\255\016\001\255\255\018\001\019\001\020\001\
\255\255\022\001\023\001\024\001\005\001\026\001\255\255\028\001\
\009\001\010\001\255\255\255\255\013\001\014\001\255\255\016\001\
\255\255\018\001\019\001\020\001\255\255\022\001\023\001\024\001\
\005\001\026\001\255\255\028\001\009\001\010\001\255\255\255\255\
\013\001\014\001\255\255\016\001\255\255\018\001\019\001\020\001\
\255\255\022\001\023\001\024\001\255\255\026\001\255\255\028\001\
\001\001\002\001\003\001\004\001\255\255\255\255\255\255\008\001\
\255\255\255\255\011\001\001\001\002\001\003\001\015\001\255\255\
\017\001\018\001\255\255\255\255\021\001\005\001\255\255\024\001\
\255\255\009\001\010\001\017\001\255\255\013\001\014\001\021\001\
\016\001\255\255\018\001\255\255\020\001\255\255\022\001\255\255\
\255\255\005\001\026\001\255\255\028\001\009\001\010\001\255\255\
\255\255\255\255\014\001\255\255\016\001\255\255\018\001\255\255\
\020\001\255\255\022\001\255\255\001\001\002\001\003\001\255\255\
\028\001\006\001\255\255\255\255\001\001\002\001\003\001\012\001\
\255\255\006\001\255\255\255\255\017\001\255\255\255\255\012\001\
\021\001\255\255\255\255\024\001\017\001\001\001\002\001\003\001\
\021\001\255\255\006\001\024\001\255\255\001\001\002\001\003\001\
\255\255\255\255\255\255\255\255\255\255\017\001\255\255\255\255\
\255\255\021\001\255\255\255\255\024\001\017\001\009\001\010\001\
\255\255\021\001\255\255\014\001\024\001\016\001\255\255\018\001\
\255\255\020\001\255\255\022\001\255\255\009\001\010\001\255\255\
\255\255\028\001\014\001\255\255\016\001\255\255\018\001\255\255\
\020\001\255\255\022\001\255\255\255\255\255\255\255\255\255\255\
\028\001"

let yynames_const = "\
  FUNCTION\000\
  PIPE\000\
  WILDCARD\000\
  FN_ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  MATCH\000\
  WITH\000\
  LBRACK\000\
  RBRACK\000\
  CONS\000\
  SEMICOLON\000\
  LPAREN\000\
  RPAREN\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  GT\000\
  COLON\000\
  EOD\000\
  SHOW_ENV\000\
  SHOW_AST\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 35 "parser.mly"
           ( print ";;"; _1 )
# 289 "parser.ml"
               : modecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                 ( print "d -> #env"; ShowEnv )
# 295 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 39 "parser.mly"
                     ( print "d -> #ast e"; ShowAst _2 )
# 302 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 40 "parser.mly"
            ( print "d -> e"; Expr(_1) )
# 309 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 41 "parser.mly"
                           ( print "d -> let p = e"; LetDecl(_2,_4) )
# 317 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 43 "parser.mly"
      ( print "d -> let rec f p = e"; LetRecDecl(_3,_5) )
# 325 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compareexp) in
    Obj.repr(
# 47 "parser.mly"
               ( _1 )
# 332 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patterns) in
    Obj.repr(
# 48 "parser.mly"
                               ( print "e -> match e with ps"; Match(_2,_4) )
# 340 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 49 "parser.mly"
                             ( print "e -> if e then e else e"; If(_2,_4,_6) )
# 349 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 50 "parser.mly"
                                  ( print "e -> function p -> e"; Fun(_2,_4) )
# 357 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 51 "parser.mly"
                              ( print "e -> let p = e"; Let(_2,_4,_6) )
# 366 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 53 "parser.mly"
      ( print "e -> let rec f p = e"; LetRec(_3,_5,_7) )
# 375 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 57 "parser.mly"
           ( _1 )
# 382 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 58 "parser.mly"
                          ( BinOp(_1,Eq,_3) )
# 390 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'compareexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 59 "parser.mly"
                          ( BinOp(_1,Gt,_3) )
# 398 "parser.ml"
               : 'compareexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'plusexp) in
    Obj.repr(
# 62 "parser.mly"
            ( _1 )
# 405 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'consexp) in
    Obj.repr(
# 63 "parser.mly"
                         ( BinOp(_1,Cons,_3) )
# 413 "parser.ml"
               : 'consexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 67 "parser.mly"
             ( _1 )
# 420 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 68 "parser.mly"
                          ( BinOp(_1,Plus,_3) )
# 428 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'plusexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'timesexp) in
    Obj.repr(
# 69 "parser.mly"
                           ( BinOp(_1,Minus,_3) )
# 436 "parser.ml"
               : 'plusexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'negexp) in
    Obj.repr(
# 73 "parser.mly"
           ( _1 )
# 443 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'timesexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 74 "parser.mly"
                          ( BinOp(_1,Times,_3) )
# 451 "parser.ml"
               : 'timesexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 78 "parser.mly"
           ( _1 )
# 458 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'appexp) in
    Obj.repr(
# 79 "parser.mly"
                 ( Negate(_2) )
# 465 "parser.ml"
               : 'negexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 83 "parser.mly"
            ( print "ae -> be"; _1 )
# 472 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'baseexp) in
    Obj.repr(
# 84 "parser.mly"
                   ( print "ae -> ae ae"; FunCall(_1,_2) )
# 480 "parser.ml"
               : 'appexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 87 "parser.mly"
        ( print "be -> c"; _1 )
# 487 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
          ( print ("be -> var "^_1); Var(_1) )
# 494 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'explist) in
    Obj.repr(
# 89 "parser.mly"
                          ( _2 )
# 501 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 90 "parser.mly"
                      ( print "be -> (e)"; _2 )
# 508 "parser.ml"
               : 'baseexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
         ( print ("c -> int "^(string_of_int _1)); IntConst(_1) )
# 515 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 95 "parser.mly"
         ( print ("c -> bool "^(string_of_bool _1)); BoolConst(_1) )
# 522 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                  ( print "c -> []"; Nil )
# 528 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "parser.mly"
      ( BinOp(_1, Cons, Nil) )
# 535 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'explist) in
    Obj.repr(
# 101 "parser.mly"
                        ( BinOp(_1, Cons, _3) )
# 543 "parser.ml"
               : 'explist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 105 "parser.mly"
                         ( print "fp -> p -> e"; [(_1,_3)] )
# 551 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 106 "parser.mly"
                                        ( print "fp -> e fpA"; (_1,_3) :: _4 )
# 560 "parser.ml"
               : 'fn_patterns))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 110 "parser.mly"
                              ( print "fpA -> | p -> e"; [(_2,_4)] )
# 568 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'pattern) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'fn_patternsAux) in
    Obj.repr(
# 111 "parser.mly"
                                             ( print "fpA -> | p -> e fpA"; (_2,_4) :: _5 )
# 577 "parser.ml"
               : 'fn_patternsAux))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'basepat) in
    Obj.repr(
# 115 "parser.mly"
            ( print "p -> bp"; _1 )
# 584 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'basepat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pattern) in
    Obj.repr(
# 116 "parser.mly"
                         ( print "p -> p1::p2"; ConsPat(_1,_3) )
# 592 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 120 "parser.mly"
          ( print "p -> c";
				match _1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Nil -> NilPat
				  | _ -> raise ConstTypeParseError )
# 604 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "parser.mly"
               ( print ("p -> -int "^(string_of_int (_2 * -1))); IntPat(_2 * -1) )
# 611 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
             ( print "p -> _"; WildcardPat )
# 617 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "parser.mly"
        ( print ("p -> var "^_1^":t"); VarPat(_1) )
# 624 "parser.ml"
               : 'basepat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    Obj.repr(
# 129 "parser.mly"
                          ( print "p -> (p)"; _2 )
# 631 "parser.ml"
               : 'basepat))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : modecl)
