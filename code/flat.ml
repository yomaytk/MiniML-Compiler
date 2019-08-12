open Pretty
module S = Syntax
module C = Closure

exception Error of string
let err s = raise (Error s)

type id = S.id
type binOp = S.binOp

(* ==== 値 ==== *)
type value =
        Var  of id
    | Fun  of id   (* NEW *)
    | IntV of int

(* ==== 式 ==== *)
type cexp =
        ValExp    of value
    | BinOp     of binOp * value * value
    | AppExp    of value * value list
    | IfExp     of value * exp * exp
    | TupleExp  of value list
    | ProjExp   of value * int

and exp =
        CompExp   of cexp
    | LetExp    of id * cexp * exp
    | LoopExp   of id * cexp * exp
    | RecurExp  of value

(* ==== 関数宣言 ==== *)
type decl = RecDecl of id * id list * exp  (* NEW *)

(* ==== プログラム ==== *)
type prog = decl list  (* NEW *)

(* ==== 関数定義のリスト ==== *)
let program = ref []
(* ==== Formatter ==== *)

let string_of_flat prog =
    let pr_of_op = function
        S.Plus -> text "+"
        | S.Mult -> text "*"
        | S.Lt -> text "<" in
    let pr_of_value = function
        Var id -> text id
        | Fun id -> text "#'" <*> text id
        | IntV i ->
            let s = text (string_of_int i) in
            if i < 0 then text "(" <*> s <*> text ")" else s
    in
    let pr_of_values = function
        [] -> text "()"
        | v :: vs' ->
            text "(" <*>
            (List.fold_left
            (fun d v -> d <*> text "," <+> pr_of_value v)
            (pr_of_value v) vs')
            <*> (text ")")
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
        | LoopExp (id, ce, e) ->
            enclose 1
            ((nest 2 (group (text "loop" <+> text id <+>
                            text "=" <|> pr_of_cexp 1 ce)))
            <+> text "in" <|> pr_of_exp 1 e)
        | RecurExp v ->
            enclose 3 (text "recur" <+> pr_of_value v)
    in
    let pr_of_decl = function
        RecDecl (id, params, body) ->
            let tparms = match params with
                [] -> text ""
            | param :: params' ->
                List.fold_left (fun t p -> t <*> text "," <+> text p)
                    (text param) params'
            in
            (group (text "let" <+> text "rec" <+>
                    (group
                    ((text id) <+> text "(" <*> tparms <*> text ")" <+>
                        nest 2 (text "=" <|> pr_of_exp 1 body)))))
    in
    layout
        (pretty 30 (List.fold_right (fun decl doc ->
            pr_of_decl decl <|> nil <|> doc) prog nil))

(* ==== フラット化：変数参照と関数参照の区別も同時に行う ==== *)

(* 参照式の環境 *)
let flat_env = ref Environment.empty 

let cv_fv v = match v with
        C.Var id -> Var id
    |   C.IntV i -> IntV i

let rec cvl_fvl vl = match vl with
        [] -> []
    |   (C.Var id) :: rest -> (cv_fv (C.Var id)) :: (cvl_fvl rest)
    |   (C.IntV i) :: rest -> (cv_fv (C.IntV i)) :: (cvl_fvl rest)

let rec cce_fce ce = match ce with
        C.ValExp v -> ValExp (cv_fv v)
    |   C.BinOp (op, v1, v2) -> BinOp (op, cv_fv v1, cv_fv v2)
    |   C.AppExp (v, vl) -> AppExp (cv_fv v, cvl_fvl vl)
    |   C.IfExp (v, e1, e2) -> IfExp (cv_fv v, flatten e1, flatten e2)
    |   C.TupleExp vl -> TupleExp (cvl_fvl vl)
    |   C.ProjExp (v, i) -> ProjExp (cv_fv v, i)
and
flatten exp = match exp with
        C.CompExp ce -> CompExp (cce_fce ce)
    |   C.LetExp (id, ce, e) -> flat_env := Environment.extend id (Var id) !flat_env;LetExp (id, cce_fce ce, flatten e)
    |   C.LetRecExp (id, idl, e1, e2) -> flat_env := Environment.extend id (Var id) !flat_env;
                                            let e = flatten e1 in
                                            program := (RecDecl (id, idl, e)) :: !program;
                                            flatten e2
    |   C.LoopExp (id, ce, e) -> LoopExp (id, cce_fce ce, flatten e)
    |   C.RecurExp v -> RecurExp (cv_fv v)
