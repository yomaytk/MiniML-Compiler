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
let con_cvalue value = match value with
        N.Var id -> Var id
    |   N.IntV i -> IntV i

(* N.expからすべての自由変数をリストとして返す *)
let rec sub_get_frv (cexp : N.cexp) (x : id) = match cexp with
        N.ValExp (Var id) -> if (x = id) || (id.[0] = '_') then [] else [Var id]
    |   N.ValExp (IntV _) -> []
    |   N.BinOp (_, v1, v2) -> (sub_get_frv (N.ValExp v1) x) @ (sub_get_frv (N.ValExp v2) x)
    |   N.AppExp (v1, v2) -> (sub_get_frv (N.ValExp v1) x) @ (sub_get_frv (N.ValExp v2) x)
    |   N.IfExp (v1, e1, e2) -> (sub_get_frv (N.ValExp v1) x) @ (get_frv e1 x) @ (get_frv e2 x)
    |   N.TupleExp (v1, v2) -> (sub_get_frv (N.ValExp v1) x) @ (sub_get_frv (N.ValExp v2) x)
    |   N.ProjExp (v, _) -> (sub_get_frv (N.ValExp v) x)
and 
get_frv (exp : N.exp) (x : id) = match exp with
        N.CompExp cexp -> sub_get_frv cexp x
    |   N.LetExp (_, cexp, e) -> (sub_get_frv cexp x) @ (get_frv e x)
    |   N.LetRecExp (_, _, e1, e2) -> (get_frv e1 x) @ (get_frv e2 x)
    |   N.LoopExp (_, cexp, e) -> (sub_get_frv cexp x) @ (get_frv e x)
    |   N.RecurExp v -> sub_get_frv (ValExp v) x

let rec make_f frvlist (f : value) (k : N.cexp -> exp) i = match frvlist with
        [] -> k
    |   (Var id) :: rest -> make_f rest f (fun x -> LetExp (id, ProjExp(f, i), k x)) (i+1)
    |   _ -> err "make_f error"

let rec con_ncexp_ccexp cexp = match cexp with
        N.ValExp value -> ValExp (con_cvalue value)
    |   N.BinOp(op, e1, e2) -> BinOp (op, con_cvalue e1, con_cvalue e2)
    |   N.AppExp (v1, v2) -> AppExp (con_cvalue v1, [con_cvalue v2])
    |   N.IfExp (e1, e2, e3) -> IfExp (con_cvalue e1, convert e2 (fun x -> CompExp (con_ncexp_ccexp x)), convert e3 (fun x -> CompExp (con_ncexp_ccexp x)))
    |   N.TupleExp (e1, e2) -> TupleExp [con_cvalue e1;con_cvalue e2]
    |   N.ProjExp (e, i) -> ProjExp (con_cvalue e, i)
and 
con_con_ncexp_ccexp cexp f = match cexp with
        N.AppExp (v1, v2) -> let fp = fresh_id "r_f" in
    let cv1 = con_cvalue v1 in
    let cv2 = con_cvalue v2 in
    LetExp (fp, ProjExp (cv1, 0), CompExp (AppExp(Var fp, [cv1;cv2])))
    |   _ -> f cexp
and
(* val convert : N.exp -> C.exp *)
convert (exp : N.exp) (f : N.cexp -> exp) = match exp with
        N.CompExp cexp -> con_con_ncexp_ccexp cexp f
    |   N.LetExp (id, e1, e2) -> LetExp (id, con_ncexp_ccexp e1, convert e2 f)
    |   N.LetRecExp (id1, id2, e1, e2) -> 
            (match e1 with
                    N.CompExp e' -> let fp = fresh_id "b_f" in
                                    let freevarlist = get_frv (N.CompExp e') id2 in
                                    let kf = make_f freevarlist (Var id1) f 1 in
                                    LetRecExp (fp, [id1;id2], kf e', LetExp (id1, TupleExp ((Var fp) :: freevarlist), convert e2 (fun x -> CompExp (con_ncexp_ccexp x))))
                |   N.LetExp (id', e1', e2') -> convert (N.LetRecExp (id1, id2, e2', e2)) (fun x -> LetExp (id', con_ncexp_ccexp e1', f x))
                |   N.LetRecExp (_, _, _, _) -> let fp = fresh_id "b_f" in
                                                            LetRecExp (fp, [id1;id2], convert e1 f, LetExp (id1, TupleExp ([Var fp]), convert e2 (fun x -> CompExp (con_ncexp_ccexp x))))
                |   N.LoopExp (id', ce', e') -> convert (N.LetRecExp (id1, id2, e', e2)) (fun x -> LoopExp (id', con_ncexp_ccexp ce', f x))
                |   N.RecurExp v -> err "closure convert recur error")
    |   N.LoopExp (id, e1, e2) -> LoopExp (id, con_ncexp_ccexp e1, convert e2 f)
    |   N.RecurExp e -> RecurExp (con_cvalue e)
    (* |   _ -> CompExp (ValExp (IntV 1)) *)
and convert_exe e = convert e (fun x -> CompExp (con_ncexp_ccexp x))

