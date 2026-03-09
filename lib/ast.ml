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

(* types *)
type typ =
  | Pointer of typ
  | TyInt
  | TyBool
  | TyFloat
  | TyChar
  | TyVoid
  | TyStruct of string

(* expressions *)
type expr =
  | IntLiteral of int
  | StrLiteral of string
  | CharLiteral of int (* chars get lowered to ints during codegen *)
  | FloatLiteral of float
  | BoolLit of bool
  | Null
  | Identifier of string (* variables *)
  | BinaryOp of op * expr * expr (* binary operators *)
  | UnaryOp of uop * expr (* unary operators *)
  | Call of string * expr list (* function calls, f(x, y, z) *)
  | Cast of typ * expr (* type casting, (long)10 *)
  | Access of expr * expr (* struct access *)
  | Deref of expr (* *pointer *)
  | Addr of expr (* &expression *)
  | Assign of expr * expr
  | Sizeof of typ
  | NoExpr (* used for dangling if's *)

(* statements *)
type statement =
  | Expr of expr
  | Block of statement list (* { statement; statement; statement; } *)
  | Return of expr (* return x; *)
  | If of expr * statement * statement (* if (expr) statement *)
  | For of expr * expr * expr * statement (* for (expr; expr; expr) statement *)
  | While of expr * statement (* while (expr) statement *)

type bind = {
  bindType : typ;
  bindName : string;
}

type struc = {
  name : string;
  fields : bind list;
}

type functio = {
  typ : typ;
  name : string;
  formals : bind list;
  locals : bind list;
  body : statement list;
}

let precedence = function
  | Mul | Div | Mod -> 1
  | Add | Sub -> 2
  | BitAnd -> 3
  | BitXor -> 4
  | BitOr -> 5
  | Less | Leq | Greater | Geq -> 6
  | Equal | Neq -> 7
  | And -> 8
  | Or -> 9

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "-"
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

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

let pp_number n = string_of_int n

let rec pp_expr ?(parent_prec = 0) = function
  | IntLiteral n -> pp_number n
  | BinaryOp (op, lhs, rhs) ->
      let current_prec = precedence op in
      let lhs_str = pp_expr ~parent_prec:current_prec lhs in
      let rhs_parent_prec =
        match op with
        | Sub | Div -> current_prec + 1
        | _ -> current_prec
      in
      let rhs_str = pp_expr ~parent_prec:rhs_parent_prec rhs in
      let rendered =
        Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str
      in
      if current_prec < parent_prec then Printf.sprintf "(%s)" rendered
      else rendered
  | _ -> failwith "unimplemented"
