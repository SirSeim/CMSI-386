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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> modecl
