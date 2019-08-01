
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | SEMISEMI
  | RPAREN
  | RECUR
  | REC
  | RARROW
  | PLUS
  | MULT
  | LT
  | LPAREN
  | LOOP
  | LET
  | INTV of (int)
  | IN
  | IF
  | ID of (Syntax.id)
  | FUN
  | FALSE
  | EQ
  | ELSE
  | DOT
  | COMMA

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.exp)
