
module MenhirBasics = struct
  
  exception Error
  
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
    | INTV of (
# 10 "parser.mly"
       (int)
# 24 "parser.ml"
  )
    | IN
    | IF
    | ID of (
# 11 "parser.mly"
       (Syntax.id)
# 31 "parser.ml"
  )
    | FUN
    | FALSE
    | EQ
    | ELSE
    | DOT
    | COMMA
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState54
  | MenhirState51
  | MenhirState49
  | MenhirState45
  | MenhirState43
  | MenhirState38
  | MenhirState28
  | MenhirState27
  | MenhirState25
  | MenhirState19
  | MenhirState15
  | MenhirState13
  | MenhirState6
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "parser.mly"
  
open Syntax

# 80 "parser.ml"

let rec _menhir_run37 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, (e : (Syntax.exp))) = _menhir_stack in
    let _3 = () in
    let _1 = () in
    let _v : (Syntax.exp) = 
# 61 "parser.mly"
                        ( e )
# 92 "parser.ml"
     in
    _menhir_goto_AExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_Expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (i : (
# 11 "parser.mly"
       (Syntax.id)
# 119 "parser.ml"
        ))), _, (e : (Syntax.exp))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 70 "parser.mly"
                         ( FunExp (i, e) )
# 126 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 24 "parser.mly"
                ( e )
# 134 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))), _, (e3 : (Syntax.exp))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 64 "parser.mly"
                                       ( IfExp (e1, e2, e3) )
# 227 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 23 "parser.mly"
               ( e )
# 235 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), (i : (
# 11 "parser.mly"
       (Syntax.id)
# 284 "parser.ml"
        ))), (p : (
# 11 "parser.mly"
       (Syntax.id)
# 288 "parser.ml"
        ))), _, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 74 "parser.mly"
   ( if i = p then
					err "Name conflict"
				else if i = "main" then
					err "main must not be declared"
				else
					LetRecExp (i, p, e1, e2) )
# 304 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 26 "parser.mly"
                ( e )
# 312 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (i : (
# 11 "parser.mly"
       (Syntax.id)
# 361 "parser.ml"
        ))), _, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 67 "parser.mly"
                                 ( LetExp (i, e1, e2) )
# 369 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 25 "parser.mly"
                ( e )
# 377 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (i : (
# 11 "parser.mly"
       (Syntax.id)
# 426 "parser.ml"
        ))), _, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 82 "parser.mly"
                                  ( LoopExp (i, e1, e2) )
# 434 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 27 "parser.mly"
                ( e )
# 442 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | RPAREN ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Syntax.exp) = 
# 45 "parser.mly"
                                      ( TupleExp (e1, e2) )
# 503 "parser.ml"
             in
            _menhir_goto_TupleExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Syntax.exp))) = _menhir_stack in
        let _1 = () in
        let _v : (Syntax.exp) = 
# 85 "parser.mly"
               ( RecurExp e )
# 520 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Syntax.exp)) = _v in
        let _v : (Syntax.exp) = 
# 28 "parser.mly"
                ( e )
# 528 "parser.ml"
         in
        _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMISEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 14 "parser.mly"
      (Syntax.exp)
# 544 "parser.ml"
            ) = 
# 18 "parser.mly"
                  ( let ast = e in
											recur_check ast false;
											ast )
# 550 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 14 "parser.mly"
      (Syntax.exp)
# 557 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_LTExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Syntax.exp)) = _v in
    let _v : (Syntax.exp) = 
# 30 "parser.mly"
                ( e )
# 577 "parser.ml"
     in
    _menhir_goto_Expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_goto_PExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState2 | MenhirState65 | MenhirState3 | MenhirState62 | MenhirState6 | MenhirState59 | MenhirState57 | MenhirState54 | MenhirState13 | MenhirState51 | MenhirState49 | MenhirState15 | MenhirState28 | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | IN | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
            let _v : (Syntax.exp) = 
# 34 "parser.mly"
           ( e )
# 637 "parser.ml"
             in
            _menhir_goto_LTExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLUS ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | IN | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.exp) = 
# 33 "parser.mly"
                       ( BinOp (Lt, e1, e2) )
# 660 "parser.ml"
             in
            _menhir_goto_LTExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAREN ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_goto_TupleExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Syntax.exp)) = _v in
    let _v : (Syntax.exp) = 
# 42 "parser.mly"
               ( e )
# 700 "parser.ml"
     in
    _menhir_goto_MExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_MExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | IN | LT | PLUS | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.exp) = 
# 37 "parser.mly"
                         ( BinOp (Plus, e1, e2) )
# 727 "parser.ml"
             in
            _menhir_goto_PExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState2 | MenhirState65 | MenhirState3 | MenhirState62 | MenhirState6 | MenhirState59 | MenhirState57 | MenhirState54 | MenhirState13 | MenhirState51 | MenhirState49 | MenhirState15 | MenhirState19 | MenhirState45 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | IN | LT | PLUS | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
            let _v : (Syntax.exp) = 
# 38 "parser.mly"
           ( e )
# 749 "parser.ml"
             in
            _menhir_goto_PExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FUN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LOOP ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | RECUR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_reduce7 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
    let _v : (Syntax.exp) = 
# 54 "parser.mly"
           ( e )
# 798 "parser.ml"
     in
    _menhir_goto_AppExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ProjExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Syntax.exp)) = _v in
    let _v : (Syntax.exp) = 
# 46 "parser.mly"
               ( e )
# 810 "parser.ml"
     in
    _menhir_goto_TupleExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_AppExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState2 | MenhirState3 | MenhirState65 | MenhirState6 | MenhirState62 | MenhirState57 | MenhirState59 | MenhirState13 | MenhirState54 | MenhirState15 | MenhirState49 | MenhirState51 | MenhirState19 | MenhirState45 | MenhirState25 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | ID _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | INTV _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | TRUE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | COMMA | ELSE | IN | LT | MULT | PLUS | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
            let _v : (Syntax.exp) = 
# 50 "parser.mly"
              ( e )
# 839 "parser.ml"
             in
            _menhir_goto_ProjExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | ID _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | INTV _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LPAREN ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | TRUE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | COMMA | ELSE | IN | LT | MULT | PLUS | RPAREN | SEMISEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Syntax.exp) = 
# 41 "parser.mly"
                           ( BinOp (Mult, e1, e2) )
# 868 "parser.ml"
             in
            _menhir_goto_MExpr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | _ ->
        _menhir_fail ()

and _menhir_goto_AExpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Syntax.exp))), _, (e2 : (Syntax.exp))) = _menhir_stack in
        let _v : (Syntax.exp) = 
# 53 "parser.mly"
                      ( AppExp (e1, e2) )
# 889 "parser.ml"
         in
        _menhir_goto_AppExpr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState2 | MenhirState3 | MenhirState65 | MenhirState6 | MenhirState62 | MenhirState57 | MenhirState59 | MenhirState13 | MenhirState54 | MenhirState15 | MenhirState49 | MenhirState51 | MenhirState19 | MenhirState45 | MenhirState25 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | INTV _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (i : (
# 10 "parser.mly"
       (int)
# 909 "parser.ml"
                )) = _v in
                let (_menhir_stack, _menhir_s, (e : (Syntax.exp))) = _menhir_stack in
                let _2 = () in
                let _v : (Syntax.exp) = 
# 49 "parser.mly"
                     ( ProjExp (e, i) )
# 916 "parser.ml"
                 in
                _menhir_goto_ProjExpr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | COMMA | ELSE | FALSE | ID _ | IN | INTV _ | LPAREN | LT | MULT | PLUS | RPAREN | SEMISEMI | THEN | TRUE ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.exp) = 
# 58 "parser.mly"
        ( BLit true )
# 1028 "parser.ml"
     in
    _menhir_goto_AExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LOOP ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | RECUR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LOOP ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RECUR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FUN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ID _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RARROW ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | FALSE ->
                                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | FUN ->
                                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | ID _v ->
                                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                            | IF ->
                                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | INTV _v ->
                                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
                            | LET ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | LOOP ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | LPAREN ->
                                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | RECUR ->
                                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | TRUE ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 1291 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 10 "parser.mly"
       (int)
# 1299 "parser.ml"
    )) = _v in
    let _v : (Syntax.exp) = 
# 57 "parser.mly"
         ( ILit i )
# 1304 "parser.ml"
     in
    _menhir_goto_AExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | FUN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LOOP ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | RECUR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (Syntax.id)
# 1342 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 11 "parser.mly"
       (Syntax.id)
# 1350 "parser.ml"
    )) = _v in
    let _v : (Syntax.exp) = 
# 60 "parser.mly"
        ( Var i )
# 1355 "parser.ml"
     in
    _menhir_goto_AExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | FUN ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | ID _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | IF ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | INTV _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LOOP ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | RECUR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Syntax.exp) = 
# 59 "parser.mly"
         ( BLit false )
# 1421 "parser.ml"
     in
    _menhir_goto_AExpr _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and toplevel : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 14 "parser.mly"
      (Syntax.exp)
# 1440 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INTV _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LOOP ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RECUR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 1482 "parser.ml"
