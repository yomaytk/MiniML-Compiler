exception Error of string
let err s = raise (Error s)

type id = string

type binOp = Plus | Mult | Lt

type exp =
    Var       of id
  | ILit      of int
  | BLit      of bool
  | BinOp     of binOp * exp * exp
  | IfExp     of exp * exp * exp
  | LetExp    of id * exp * exp
  | FunExp    of id * exp
  | AppExp    of exp * exp
  | LetRecExp of id * id * exp * exp
  | LoopExp   of id * exp * exp (* loop <id> = <exp> in <exp> *)
  | RecurExp  of exp            (* recur <exp> *)
  | TupleExp  of exp * exp      (* (<exp>, <exp>) *)
  | ProjExp   of exp * int      (* <exp> . <int> *)

(* ==== recur式が末尾位置にのみ書かれていることを検査 ==== *)

let rec recur_check exp f =  
  match exp with
      RecurExp e -> if f then () else err ("recur_check error")
    | BinOp (_, e1, e2) -> recur_check e1 false;recur_check e2 false
    | IfExp (e1, e2, e3) -> recur_check e1 false;recur_check e2 f;recur_check e3 f
    | LetExp (_, e1, e2) -> recur_check e1 false;recur_check e2 f
    | FunExp (_, e) -> recur_check e false
    | AppExp (e1, e2) -> recur_check e1 false;recur_check e2 false
    | LetRecExp (_, _, e1, e2) -> recur_check e1 false;recur_check e2 f
    | LoopExp (_, e1, e2) -> recur_check e1 false;recur_check e2 true
    | TupleExp (e1, e2) -> recur_check e1 false;recur_check e2 false
    | ProjExp (e, _) -> recur_check e false
    | _ -> ()
