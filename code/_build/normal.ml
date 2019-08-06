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
    | S.BLit bl -> if bl then IntV 1 else IntV 0
    | _ -> err "con_expvalue error"

let rec norm_exp (e: Syntax.exp) (f: cexp -> exp) = match e with
    S.Var id -> f (ValExp (Var id))
  | S.ILit i -> f (ValExp (IntV i))
  | S.BLit true -> f (ValExp (IntV 1))
  | S.BLit false -> f (ValExp (IntV 0))
  | S.BinOp(op, e1, e2) -> 
      let nid1 = fresh_id "va" in
      let nid2 = fresh_id "va" in
      norm_exp e1 (fun x -> (norm_exp e2 (fun y -> LetExp (nid1, x, LetExp (nid2, y, f (BinOp(op, Var nid1, Var nid2)))))))
  | S.IfExp (e1, e2, e3) ->
      let nid = fresh_id "va" in
      norm_exp e1 (fun x -> LetExp (nid, x, f (IfExp (Var nid, norm_exp e2 f, norm_exp e3 f))))
  | S.LetExp (id, e1, e2) -> 
      (match e1 with
          S.LetExp (_, _, _) | S.LetRecExp (_, _, _, _) | S.LoopExp(_, _, _) | S.RecurExp _ -> 
            let nid = fresh_id "va" in
            norm_exp e1 (fun x -> LetExp (nid, x, LetExp(id, ValExp (Var nid), norm_exp e2 f)))
        | _ -> norm_exp e1 (fun x -> LetExp (id, x, norm_exp e2 f)))
  | S.FunExp (id, e) -> let ff = fresh_id "f" in norm_exp (S.LetRecExp (ff, id, e, Var ff)) f
  | S.AppExp (e1, e2) -> 
      let nid1 = fresh_id "va" in
      let nid2 = fresh_id "va" in
      norm_exp e1 (fun x -> norm_exp e2 (fun y -> LetExp (nid1, x, LetExp (nid2, y, f (AppExp (Var nid1, Var nid2))))))
  | S.LetRecExp (id1, id2, e1, e2) -> LetRecExp (id1, id2, norm_exp e1 f, norm_exp e2 f)
  | S.LoopExp (id, e1, e2) -> 
      (match e1 with
          S.LetExp (_, _, _) | S.LetRecExp (_, _, _, _) | S.LoopExp (_, _, _) | S.RecurExp _ ->
            let nid = fresh_id "va" in
            norm_exp e1 (fun x -> LetExp (nid, x, LoopExp (id, ValExp (Var nid), norm_exp e2 f)))
        | _ -> norm_exp e1 (fun x -> LoopExp (id, x, norm_exp e2 f)))
  | S.RecurExp e -> let nid = fresh_id "va" in norm_exp e (fun x -> LetExp (nid, x, RecurExp (Var nid)))
  | S.TupleExp (e1, e2) -> 
      let nid1 = fresh_id "va" in
      let nid2 = fresh_id "va" in
      norm_exp e1 (fun x -> norm_exp e2 (fun y -> LetExp (nid1, x, LetExp (nid2, y, f (TupleExp (Var nid1, Var nid2))))))
  | S.ProjExp (e, i) ->
      let nid = fresh_id "va" in
      norm_exp e (fun x -> LetExp (nid, x, f (ProjExp (Var nid, i))))
  (* | _ -> f (ValExp (IntV 1))   TODO *)

and normalize e = norm_exp e (fun ce -> CompExp ce)

(* ==== entry point ==== *)
let convert prog =
  normalize prog
