%{
exception ConstTypeParseError

let print_productions = false
let print p = if print_productions then print_string (p^"\n") else ()
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR

%token FUNCTION
%token PIPE
%token WILDCARD
%token FN_ARROW
%token IF THEN ELSE
%token LET REC EQ IN
%token MATCH WITH
%token LBRACK RBRACK CONS SEMICOLON
%token LPAREN RPAREN
%token PLUS MINUS TIMES GT
%token COLON
%token EOD SHOW_ENV SHOW_AST

%right FN_ARROW
%left COMMA
%left COLON
%left PLUS
%left PIPE
%right CONS

%start main
%type <modecl> main
%%
main: decl { print ";;"; $1 }

decl:
    SHOW_ENV EOD { print "d -> #env"; ShowEnv }
|   SHOW_AST exp EOD { print "d -> #ast e"; ShowAst $2 }
|   exp EOD { print "d -> e"; Expr($1) }
|   LET pattern EQ exp EOD { print "d -> let p = e"; LetDecl($2,$4) }
|   LET REC VAR EQ exp EOD
      { print "d -> let rec f p = e"; LetRecDecl($3,$5) }
;

exp:
    compareexp { $1 }
  | MATCH exp WITH fn_patterns { print "e -> match e with ps"; Match($2,$4) }
  | IF exp THEN exp ELSE exp { print "e -> if e then e else e"; If($2,$4,$6) }
  | FUNCTION pattern FN_ARROW exp { print "e -> function p -> e"; Fun($2,$4) }
  | LET pattern EQ exp IN exp { print "e -> let p = e"; Let($2,$4,$6) }
  | LET REC VAR EQ exp IN exp
      { print "e -> let rec f p = e"; LetRec($3,$5,$7) }
;

compareexp:
   consexp { $1 }
  | compareexp EQ consexp { BinOp($1,Eq,$3) }
  | compareexp GT consexp { BinOp($1,Gt,$3) }
      
consexp:
    plusexp { $1 }
  | plusexp CONS consexp { BinOp($1,Cons,$3) }
;

plusexp:
    timesexp { $1 }
  | plusexp PLUS timesexp { BinOp($1,Plus,$3) }
  | plusexp MINUS timesexp { BinOp($1,Minus,$3) }
;

timesexp:
    negexp { $1 }
  | timesexp TIMES appexp { BinOp($1,Times,$3) }
;

negexp:
    appexp { $1 }
  | MINUS appexp { Negate($2) }
;

appexp:
    baseexp { print "ae -> be"; $1 }
  | appexp baseexp { print "ae -> ae ae"; FunCall($1,$2) }

baseexp:      
  const { print "be -> c"; $1 }
  | VAR   { print ("be -> var "^$1); Var($1) }
  | LBRACK explist RBRACK { $2 }
  | LPAREN exp RPAREN { print "be -> (e)"; $2 }
;

const:
    INT  { print ("c -> int "^(string_of_int $1)); IntConst($1) }
  | BOOL { print ("c -> bool "^(string_of_bool $1)); BoolConst($1) }
  | LBRACK RBRACK { print "c -> []"; Nil }
;

explist:
  exp { BinOp($1, Cons, Nil) }
| exp SEMICOLON explist { BinOp($1, Cons, $3) }
;

fn_patterns:
    pattern FN_ARROW exp { print "fp -> p -> e"; [($1,$3)] }
  | pattern FN_ARROW exp fn_patternsAux { print "fp -> e fpA"; ($1,$3) :: $4 }
;

fn_patternsAux:
    PIPE pattern FN_ARROW exp { print "fpA -> | p -> e"; [($2,$4)] }
  | PIPE pattern FN_ARROW exp fn_patternsAux { print "fpA -> | p -> e fpA"; ($2,$4) :: $5 }
;

pattern:
    basepat { print "p -> bp"; $1 }
  | basepat CONS pattern { print "p -> p1::p2"; ConsPat($1,$3) }
;    

basepat:
    const { print "p -> c";
				match $1 with
					 IntConst(i)  -> IntPat(i)
				  | BoolConst(b) -> BoolPat(b)
				  | Nil -> NilPat
				  | _ -> raise ConstTypeParseError }
  | MINUS INT  { print ("p -> -int "^(string_of_int ($2 * -1))); IntPat($2 * -1) }
  | WILDCARD { print "p -> _"; WildcardPat }
  | VAR { print ("p -> var "^$1^":t"); VarPat($1) }
  | LPAREN pattern RPAREN { print "p -> (p)"; $2 }
;
