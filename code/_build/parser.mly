%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI RARROW
%token PLUS MULT LT EQ
%token IF THEN ELSE TRUE FALSE LET IN FUN REC
%token LOOP RECUR COMMA DOT

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.exp> toplevel
%%

toplevel :
		e=Expr SEMISEMI { let ast = e in
											recur_check ast false;
											ast }

Expr :
		e=IfExpr     { e }
	| e=FunExpr    { e }
	| e=LetExpr    { e }
	| e=LetRecExpr { e }
	| e=LoopExpr   { e }
	| e=RecurExpr  { e }
	| e=TupleExpr  { e }
	| e=ProjExpr   { e }
	| e=LTExpr     { e }

LTExpr :
		e1=PExpr LT e2=PExpr { BinOp (Lt, e1, e2) }
	| e=PExpr { e }

PExpr :
		e1=PExpr PLUS e2=MExpr { BinOp (Plus, e1, e2) }
	| e=MExpr { e }

MExpr :
		e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
	| e=AppExpr { e }

AppExpr :
		e1=AppExpr e2=AExpr { AppExp (e1, e2) }
	| e=AExpr { e }

AExpr :
		i=INTV { ILit i }
	| TRUE { BLit true }
	| FALSE { BLit false }
	| i=ID { Var i }
	| LPAREN e=Expr RPAREN { e }

IfExpr :
		IF e1=Expr THEN e2=Expr ELSE e3=Expr { IfExp (e1, e2, e3) }

LetExpr :
		LET i=ID EQ e1=Expr IN e2=Expr { LetExp (i, e1, e2) }

FunExpr :
		FUN i=ID RARROW e=Expr { FunExp (i, e) }

LetRecExpr :
		LET REC i=ID EQ FUN p=ID RARROW e1=Expr IN e2=Expr
			{ if i = p then
					err "Name conflict"
				else if i = "main" then
					err "main must not be declared"
				else
					LetRecExp (i, p, e1, e2) }

LoopExpr :
		LOOP i=ID EQ e1=Expr IN e2=Expr { LoopExp (i, e1, e2) }

RecurExpr :
		RECUR e=Expr { RecurExp e }

TupleExpr :
		LPAREN e1=Expr COMMA e2=Expr RPAREN { TupleExp (e1, e2) }

ProjExpr :
		e=AExpr DOT i=INTV { ProjExp (e, i) }