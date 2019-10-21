open Pretty
module S = Syntax

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

let fresh_id = Misc.fresh_id_maker "_"

(* ==== 値 ==== *)
type value =
	Var  of id
	| IntV of int

(* ==== 式 ==== *)
type cexp =
        ValExp    of value
	| BinOp     of binOp * value * value
	| AppExp    of value * value
	| IfExp     of value * exp * exp
	| TupleExp  of value * value
	| ProjExp   of value * int

and exp =
        CompExp   of cexp
	| LetExp    of id * cexp * exp
	| LetRecExp of id * id * exp * exp
	| LoopExp   of id * cexp * exp
	| RecurExp  of value

(* ==== Formatter ==== *)

let string_of_norm e =
	let pr_of_op = function
	S.Plus -> text "+"
	| S.Mult -> text "*"
	| S.Lt -> text "<" in
	let pr_of_value = function
	Var id -> text id
	| IntV i ->
	let s = text (string_of_int i) in
	if i < 0 then text "(" <*> s <*> text ")" else s
	in
	let rec pr_of_cexp p e =
	let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
	match e with
	ValExp v -> pr_of_value v
	| BinOp (op, v1, v2) ->
	enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
	| AppExp (f, v) ->
	enclose 3 (pr_of_value f <+> pr_of_value v)
	| IfExp (v, e1, e2) ->
	enclose 1
		((nest 2
		(group ((text "if 0 <")
			<+> pr_of_value v
			<+> text "then"
			<|> pr_of_exp 1 e1))) <|>
		(nest 2
		(group (text "else" <|> pr_of_exp 1 e2))))
	| TupleExp (v1, v2) ->
	text "(" <*> pr_of_value v1 <*> text ","
	<+> pr_of_value v2 <*> text ")"
	| ProjExp (v, i) ->
	enclose 2 (pr_of_value v <*> text "." <*> text (string_of_int i))
	and pr_of_exp p e =
	let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
	match e with
	CompExp ce -> pr_of_cexp p ce
	| LetExp (id, ce, e) ->
	enclose 1
		((nest 2 (group (text "let" <+> text id <+>
				text "=" <|> pr_of_cexp 1 ce)))
		<+> text "in" <|> pr_of_exp 1 e)
	| LetRecExp (id, v, body, e) ->
	enclose 1
		((nest 2 (group (text "let" <+> text "rec" <+>
				text id <+> text v <+>
				text "=" <|> pr_of_exp 1 body)))
		<+> text "in" <|> pr_of_exp 1 e)
	| LoopExp (id, ce, e) ->
	enclose 1
		((nest 2 (group (text "loop" <+> text id <+>
				text "=" <|> pr_of_cexp 1 ce)))
		<+> text "in" <|> pr_of_exp 1 e)
	| RecurExp v ->
	enclose 3 (text "recur" <+> pr_of_value v)
	in layout (pretty 30 (pr_of_exp 0 e))


(* ==== 正規形への変換 ==== *)
let con_expvalue (e : Syntax.exp) = 
	match e with
		S.Var id -> Var id
	| S.ILit i -> IntV i
	| S.BLit true -> IntV 1
	| S.BLit false -> IntV 0
	| _ -> err "con_expvalue error"


let rec norm_exp (e: Syntax.exp) (f: cexp -> exp) = match e with

    | S.Var id -> f (ValExp (Var id))

    | S.ILit i -> f (ValExp (IntV i))

    | S.BLit true -> f (ValExp (IntV 1))

    | S.BLit false -> f (ValExp (IntV 0))

    | S.BinOp(op, e1, e2) -> let nid1 = fresh_id "ai" in let nid2 = fresh_id "bi" in
            norm_exp e1 (fun x -> 
                (match x with
                        ValExp vc1 -> norm_exp e2 (fun y -> 
                                        (match y with
                                                ValExp vc2 -> f (BinOp(op, vc1, vc2))
                                            |   _          -> LetExp(nid2, y, f (BinOp(op, vc1, Var nid2)))))
                    |   _          -> norm_exp e2 (fun y -> 
                                        (match y with
                                                ValExp vc2 -> LetExp(nid1, x, f (BinOp(op, Var nid1, vc2)))
                                            |   _          -> LetExp(nid1, x, LetExp(nid2, y, f (BinOp(op, Var nid1, Var nid2))))))))

	| S.IfExp (e1, e2, e3) ->	
        (match e1 with
                S.Var id -> f (IfExp (Var id, norm_exp e2 (fun x -> CompExp x), norm_exp e3 (fun x -> CompExp x)))
            |   S.BLit _ -> f (IfExp (con_expvalue e1, norm_exp e2 (fun x -> CompExp x), norm_exp e3 (fun x -> CompExp x)))
            | 	S.ILit _ -> err "e1 must be bool in if e1 then ..."
            | 	_ ->	let nid = fresh_id "di" in
                            norm_exp e1 (fun x -> LetExp (nid, x, f (IfExp (Var nid, 
                                norm_exp e2 (fun x -> CompExp x), norm_exp e3 (fun x -> CompExp x))))))

    | S.LetExp (id, e1, e2) -> norm_exp e1 (fun x -> LetExp (id, x, norm_exp e2 f))

    | S.FunExp (id, e) -> let ff = fresh_id "nf" in norm_exp (S.LetRecExp (ff, id, e, Var ff)) f

	| S.AppExp (e1, e2) -> 
		(match e1 with
                S.ILit _ | S.BLit _ -> err "e1 must be fun in e1 e2"
            |   S.Var id -> let nid2 = fresh_id "ei" in
                                norm_exp e2 (fun y -> 
                                    (match y with
                                            ValExp vc -> f (AppExp (Var id, vc))
                                        |   _         -> LetExp (nid2, y, f (AppExp (Var id, Var nid2)))))
			|	_ -> let nid1 = fresh_id "fi" in let nid2 = fresh_id "gi" in
                        norm_exp e1 (fun x -> 
                            norm_exp e2 (fun y -> 
                                (match y with
                                        ValExp vc -> LetExp (nid1, x, f (AppExp (Var nid1, vc)))
                                    |   _         -> LetExp (nid1, x, LetExp (nid2, y, f (AppExp (Var nid1, Var nid2))))))))

    | S.LetRecExp (id1, id2, e1, e2) -> LetRecExp (id1, id2, norm_exp e1 (fun x -> CompExp x), norm_exp e2 f)

	| S.LoopExp (id, e1, e2) -> 
        (match e1 with
                S.LetExp (_, _, _) | S.LetRecExp (_, _, _, _) | S.LoopExp (_, _, _) | S.RecurExp _ ->
                    let nid = fresh_id "va" in
                    norm_exp e1 (fun x -> LetExp (nid, x, LoopExp (id, ValExp (Var nid), norm_exp e2 f)))
            | _ -> norm_exp e1 (fun x -> LoopExp (id, x, norm_exp e2 f)))
    | S.RecurExp e -> 
        (match e with
                S.ILit _ | S.BLit _ -> RecurExp (con_expvalue e)
            |   _ -> let nid = fresh_id "va" in 
                        norm_exp e (fun x -> LetExp (nid, x, RecurExp (Var nid))))
    | S.TupleExp (e1, e2) -> 
        (match e1 with
                S.ILit _ | S.BLit _ -> let e1v = con_expvalue e1 in
                                                    (match e2 with
                                                            S.ILit _ | S.BLit _ -> f (TupleExp (e1v, con_expvalue e2))
                                                        |   _ -> let nid2 = fresh_id "va" in
                                                                    norm_exp e2 (fun x -> LetExp (nid2, x, f (TupleExp (e1v, Var nid2)))))
            |   _ -> let nid1 = fresh_id "va" in
                        (match e2 with
                                S.ILit _ | S.BLit _ -> norm_exp e1 (fun x -> LetExp (nid1, x, f (TupleExp (Var nid1, con_expvalue e2))))
                            |   _ -> let nid2 = fresh_id "va" in
                                        norm_exp e1 (fun x -> norm_exp e2 (fun y -> LetExp (nid1, x, LetExp (nid2, y, f (TupleExp (Var nid1, Var nid2))))))))
    | S.ProjExp (e, i) ->
        (match e with
                S.ILit _ | S.BLit _ -> f (ProjExp (con_expvalue e, i))
            |   _ -> let nid = fresh_id "va" in
                        norm_exp e (fun x -> LetExp (nid, x, f (ProjExp (Var nid, i)))))
	(* | _ -> f (ValExp (IntV 1))   TODO *)

and normalize e = norm_exp e (fun x -> CompExp x)

(* ==== entry point ==== *)
let convert prog =
	normalize prog
