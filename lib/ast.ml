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

type var_type = VarType of string

let string_of_var_type (VarType name) = name

(* expressions *)
type expr =
  | IntLiteral of int
  | BoolLiteral of bool
  | VarRef of string
  | BinaryOp of op * expr * expr
  | UnaryOp of uop * expr
  | Ternary of expr * expr * expr
  | FuncCall of {
      name : string;
      args : expr list;
    }
  | Assign of {
      name : string;
      value : expr;
    }

type stmt =
  | ExprStmt of expr
  | ReturnStmt of expr option
  | EmptyStmt
  | CompoundStmt of stmt list
  | VarDef of {
      var_type : var_type;
      name : string;
      init : expr;
    }
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : stmt list;
    }
  | If of {
      cond : expr;
      then_body : stmt;
      else_body : stmt option;
    }
  | WhileLoop of {
      cond : expr;
      body : stmt;
    }
  | ForLoop of {
      init : stmt;
      cond : expr;
      incr : expr;
      body : stmt;
    }

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
