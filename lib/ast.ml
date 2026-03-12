(* binary operators *)
type op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor

(* unary operators *)
type uop =
  | Neg
  | Not

(* expressions *)
type expr =
  | IntLiteral of int
  | BoolLiteral of bool
  | BinaryOp of op * expr * expr (* binary operators *)
  | UnaryOp of uop * expr (* unary operators *)
  | Statement of expr (* complete statement (currently just an expr) *)
  | EmptyStmt
  | CompoundStmt of expr list (* sequence of statements surrounded by braces *)

let precedence = function
  | Or -> 10
  | And -> 20
  | Equal | Neq -> 30
  | Less | Leq | Greater | Geq -> 40
  | BitOr -> 50
  | BitXor -> 60
  | BitAnd -> 70
  | Add | Sub -> 80
  | Mul | Div | Mod -> 90

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"

let op_of_string_opt = function
  | "+" -> Some Add
  | "-" -> Some Sub
  | "*" -> Some Mul
  | "/" -> Some Div
  | "%" -> Some Mod
  | "<" -> Some Less
  | ">" -> Some Greater
  | "==" -> Some Equal
  | "!=" -> Some Neq
  | "<=" -> Some Leq
  | ">=" -> Some Geq
  | "&&" -> Some And
  | "||" -> Some Or
  | "&" -> Some BitAnd
  | "|" -> Some BitOr
  | "^" -> Some BitXor
  | _ -> None [@coverage off]

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

let rec pp_expr ?(parent_prec = 0) ?(top_level = true) = function
  | IntLiteral n -> string_of_int n
  | BoolLiteral b -> string_of_bool b
  | UnaryOp (op, e) ->
      let e_str =
        match e with
        | BinaryOp _ | UnaryOp _ -> "(" ^ pp_expr ~top_level:false e ^ ")"
        | _ -> pp_expr ~top_level:false e
      in
      string_of_uop op ^ e_str
  | BinaryOp (op, lhs, rhs) ->
      let prec = precedence op in
      let lhs_str = pp_expr ~parent_prec:prec ~top_level:false lhs in
      let rhs_prec =
        match op with
        | Sub | Div | Mod -> prec + 1
        | _ -> prec
      in
      let rhs_str = pp_expr ~parent_prec:rhs_prec ~top_level:false rhs in
      let s = Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str in
      if prec < parent_prec then "(" ^ s ^ ")" else s
  | Statement e -> pp_expr ~top_level:false e ^ ";"
  | EmptyStmt -> ";"
  | CompoundStmt statements ->
      let body =
        String.concat " " (List.map (pp_expr ~top_level:false) statements)
      in
      if top_level then body else "{" ^ body ^ "}"
