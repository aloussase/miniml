type ty = TInt | TBool | TArrow of ty * ty

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) -> string_of_type t1 ^ " -> " ^ string_of_type t2

type expr =
  | EInt of int
  | EBool of bool
  | EIdent of string
  | EApp of expr * expr
  | EPlus of expr * expr
  | ETimes of expr * expr
  | ELess of expr * expr
  | EMinus of expr * expr
  | EEqual of expr * expr
  | EIf of expr * expr * expr
  | EFun of string * string * ty * ty * expr

let rec string_of_expr = function
  | EInt n -> string_of_int n
  | EBool b -> string_of_bool b
  | EIdent s -> s
  | EApp (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | EPlus (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | ETimes (e1, e2) -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
  | ELess (e1, e2) -> "(" ^ string_of_expr e1 ^ " < " ^ string_of_expr e2 ^ ")"
  | EMinus (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  | EEqual (e1, e2) -> "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
  | EIf (cond, then_, else_) ->
      "if " ^ string_of_expr cond ^ " then " ^ string_of_expr then_ ^ " else "
      ^ string_of_expr else_
  | EFun (name, _, _, _, _) -> "<function: " ^ name ^ ">"

type stmt = SLet of string * expr | SExpr of expr

let string_of_stmt = function
  | SLet (ident, e) -> "let " ^ ident ^ " = " ^ string_of_expr e
  | SExpr e -> string_of_expr e
