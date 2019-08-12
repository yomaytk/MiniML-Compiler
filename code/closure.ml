open Pretty
module S = Syntax
module N = Normal

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

let fresh_id = N.fresh_id

(* ==== 値 ==== *)
type value =
        Var  of id
    | IntV of int

(* ==== 式 ==== *)
type cexp =
        ValExp    of value
    | BinOp     of binOp * value * value
    | AppExp    of value * value list     (* NEW *)
    | IfExp     of value * exp * exp
    | TupleExp  of value list             (* NEW *)
    | ProjExp   of value * int

and exp =
        CompExp   of cexp
    | LetExp    of id * cexp * exp
    | LetRecExp of id * id list * exp * exp  (* NEW *)
    | LoopExp   of id * cexp * exp
    | RecurExp  of value

(* ==== Formatter ==== *)

let string_of_closure e =
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
    let pr_of_values = function
        [] -> text "()"
        | v :: vs' ->
            (text "(" <*>
            (List.fold_left
                (fun d v -> d <*> text "," <+> pr_of_value v)
                (pr_of_value v) vs')
            <*> (text ")"))
    in
    let pr_of_ids = function
        [] -> text "()"
        | id :: ids' ->
            (text "(" <*>
            (List.fold_left
                (fun d i -> d <*> text "," <+> text i)
                (text id) ids')
            <*> (text ")"))
    in
    let rec pr_of_cexp p e =
        let enclose p' e = if p' < p then text "(" <*> e <*> text ")" else e in
        match e with
        ValExp v -> pr_of_value v
        | BinOp (op, v1, v2) ->
            enclose 2 (pr_of_value v1 <+> pr_of_op op <+> pr_of_value v2)
        | AppExp (f, vs) ->
            enclose 3 (pr_of_value f <+> pr_of_values vs)
        | IfExp (v, e1, e2) ->
            enclose 1
            ((nest 2
                (group ((text "if 0 <")
                        <+> pr_of_value v
                        <+> text "then"
                        <|> pr_of_exp 1 e1))) <|>
            (nest 2
                (group (text "else" <|> pr_of_exp 1 e2))))
        | TupleExp vs -> pr_of_values vs
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
        | LetRecExp (id, parms, body, e) ->
            enclose 1
            ((nest 2 (group (text "let" <+> text "rec" <+> text id <+>
                            pr_of_ids parms <+>
                            text "=" <|> pr_of_exp 1 body)))
            <+> text "in" <|> pr_of_exp 1 e)
        | LoopExp (id, ce, e) ->
            enclose 1
            ((nest 2 (group (text "loop" <+> text id <+>
                            text "=" <|> pr_of_cexp 1 ce)))
            <+> text "in" <|> pr_of_exp 1 e)
        | RecurExp v ->
            enclose 3 (text "recur" <+> pr_of_value v)
    in layout (pretty 40 (pr_of_exp 0 e))



(* entry point *)
let c_cv value = match value with
        N.Var id -> Var id
    |   N.IntV i -> IntV i

(* N.expからすべての自由変数をリストとして返す *)
let rec sg_fv (cexp : N.cexp) (x : id) = match cexp with
        N.ValExp (Var id) -> if (x = id) || (id.[0] = '_') then [] else [Var id]
    |   N.ValExp (IntV _) -> []
    |   N.BinOp (_, v1, v2) -> (sg_fv (N.ValExp v1) x) @ (sg_fv (N.ValExp v2) x)
    |   N.AppExp (v1, v2) -> (sg_fv (N.ValExp v1) x) @ (sg_fv (N.ValExp v2) x)
    |   N.IfExp (v1, e1, e2) -> (sg_fv (N.ValExp v1) x) @ (g_fv e1 x) @ (g_fv e2 x)
    |   N.TupleExp (v1, v2) -> (sg_fv (N.ValExp v1) x) @ (sg_fv (N.ValExp v2) x)
    |   N.ProjExp (v, _) -> (sg_fv (N.ValExp v) x)
and 
g_fv (exp : N.exp) (x : id) = match exp with
        N.CompExp cexp -> sg_fv cexp x
    |   N.LetExp (id, cexp, e) -> (sg_fv (N.ValExp (Var id)) x) @ (sg_fv cexp x) @ (g_fv e x)
    |   N.LetRecExp (_, _, e1, e2) -> (g_fv e1 x) @ (g_fv e2 x) (* 入れ子の場合の処理を後で書く*)
    |   N.LoopExp (_, cexp, e) -> (sg_fv cexp x) @ (g_fv e x)
    |   N.RecurExp v -> sg_fv (N.ValExp v) x

let rec mk_f fvl (f : value) (k : N.cexp -> exp) i = match fvl with
        [] -> k
    |   (Var id) :: rest -> mk_f rest f (fun x -> LetExp (id, ProjExp(f, i), k x)) (i+1)
    |   _ -> err "mk_f error"

let rec setl (l : value list) = match l with
        [] -> []
    |   id :: rest -> if List.mem id rest then (setl rest) else id :: (setl rest) 

let rec nce_cce cexp = match cexp with
        N.ValExp value -> ValExp (c_cv value)
    |   N.BinOp(op, e1, e2) -> BinOp (op, c_cv e1, c_cv e2)
    |   N.IfExp (e1, e2, e3) -> IfExp (c_cv e1, convert e2 (fun x -> CompExp (nce_cce x)) [], convert e3 (fun x -> CompExp (nce_cce x)) [])
    |   N.TupleExp (e1, e2) -> TupleExp [c_cv e1;c_cv e2]
    |   N.ProjExp (e, i) -> ProjExp (c_cv e, i)
    |   _ -> err "nce_cce error"
and 
nce_cce2 cexp f exp fl = match cexp with
        N.AppExp (v1, v2) -> let fp = fresh_id "r_f" in
                                let cv1 = c_cv v1 in
                                let cv2 = c_cv v2 in
                                let pr = ProjExp (cv1, 0) in
                                (match exp with
                                        N.CompExp _ | N.LetRecExp (_, _, _, _) -> LetExp (fp, pr, CompExp (AppExp (Var fp, [cv1;cv2])))
                                    |   N.LetExp(id, ce, e) -> LetExp (fp, pr, LetExp (id, (AppExp (Var fp, [cv1;cv2])), convert e f fl))
                                    |   N.LoopExp (id, ce, e) -> LetExp (fp, pr, LoopExp (id, (AppExp (Var fp, [cv1;cv2])), convert e f fl))
                                    |   _ -> err "nce_cce2 error")
    |   _ -> (match exp with
                    N.CompExp ce -> f ce
                |   N.LetExp (id, ce, e) -> LetExp (id, nce_cce ce, convert e f fl)
                |   N.LoopExp (id, ce, e) -> LoopExp (id, nce_cce ce, convert e f fl)
                |   N.RecurExp v -> RecurExp (c_cv v)
                |   _ -> err "nce_cce2___ error")
and
(* val convert : N.exp -> C.exp *)
convert (exp : N.exp) (f : N.cexp -> exp) fl = match exp with
        N.CompExp ce -> nce_cce2 ce f exp fl
    |   N.LetExp (id, ce, e) -> nce_cce2 ce f exp fl
    |   N.LetRecExp (id1, id2, e1, e2) -> 
            (match e1 with
                    N.CompExp e' -> let fvl = g_fv e1 id2 in
                                    let afvl = setl (fvl @ fl) in
                                    let kf = mk_f afvl (Var id1) f 1 in
                                    let fp = fresh_id "b_f" in
                                    LetRecExp (fp, [id1;id2], kf e', LetExp (id1, TupleExp ((Var fp) :: afvl), convert e2 (fun x -> match x with
                                                                                                                                            N.AppExp (_, _) -> nce_cce2 x f e2 []
                                                                                                                                        |   _ -> CompExp (nce_cce x)) fl))
                |   N.LetExp (id', ce1', e2') -> let fvl = g_fv e1 id2 in
                                                convert (N.LetRecExp (id1, id2, e2', e2)) (fun x -> match x with
                                                                                                            N.AppExp(_, _) -> nce_cce2 x f e1 fl
                                                                                                        |   _ -> LetExp (id', nce_cce ce1', f x)) (fl @ fvl)
                |   N.LetRecExp (_, _, _, _) -> let fp = fresh_id "b_f" in
                                                            LetRecExp (fp, [id1;id2], convert e1 f [], LetExp (id1, TupleExp ([Var fp]), convert e2 f fl))
                |   N.LoopExp (id', ce', e') -> let fvl = sg_fv ce' id2 in
                                                convert (N.LetRecExp (id1, id2, e', e2)) (fun x -> match x with
                                                                                                            N.AppExp (_, _) -> nce_cce2 x f e1 fl
                                                                                                        |   _ -> LoopExp (id', nce_cce ce', f x)) (fvl @ fl)
                |   N.RecurExp v -> let fvl = g_fv e1 id2 in
                                    let afvl = setl (fvl @ fl) in
                                    let kf = mk_f afvl (Var id1) f 1 in
                                    let fp = fresh_id "b_f" in
                                    LetRecExp (fp, [id1;id2], kf (ValExp v), LetExp (id1, TupleExp ((Var fp) :: afvl), convert e2 (fun x -> nce_cce2 x f e1 []) fl)))
    |   N.LoopExp (id, ce, e) -> nce_cce2 ce f exp fl
    |   N.RecurExp e -> RecurExp (c_cv e)
and convert_exe e = convert e (fun x -> match x with
                                                N.AppExp (_, _) -> nce_cce2 x (fun y -> CompExp (nce_cce y)) e []
                                            |   _ -> CompExp (nce_cce x)) []