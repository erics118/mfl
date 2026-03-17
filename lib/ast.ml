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
  | LShift
  | RShift

(* unary operators *)
type uop =
  | Neg
  | Not
  | Compl

type var_type = VarType of string

let string_of_var_type (VarType name) = name

(* source position *)
type pos = {
  line : int;
  col : int;
}

(* types inferred/checked by the typechecker *)
type typ =
  | Int
  | Bool
  | Void

(* phantom types marking which compiler phase produced an expr *)
type parsed
type checked

(* annotation carried by every expr node; Parsed holds only position, Checked
   adds the resolved type *)
type _ ann =
  | Parsed : pos -> parsed ann
  | Checked : pos * typ -> checked ann

let dummy_pos = { line = 0; col = 0 }

let pos_of : type a. a ann -> pos = function
  | Parsed p -> p
  | Checked (p, _) -> p

let typ_of : checked ann -> typ = function
  | Checked (_, t) -> t

(* expressions *)
type 'a expr =
  | IntLiteral : 'a ann * int -> 'a expr
  | BoolLiteral : 'a ann * bool -> 'a expr
  | VarRef : 'a ann * string -> 'a expr
  | BinaryOp : 'a ann * op * 'a expr * 'a expr -> 'a expr
  | UnaryOp : 'a ann * uop * 'a expr -> 'a expr
  | Ternary : 'a ann * 'a expr * 'a expr * 'a expr -> 'a expr
  | FuncCall : 'a ann * string * 'a expr list -> 'a expr
  | Assign : 'a ann * string * 'a expr -> 'a expr

type 'a stmt =
  | ExprStmt of 'a expr
  | ReturnStmt of 'a expr option
  | EmptyStmt
  | CompoundStmt of 'a stmt list
  | VarDef of {
      var_type : var_type;
      name : string;
      init : 'a expr;
    }
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : 'a stmt list;
    }
  | If of {
      cond : 'a expr;
      then_body : 'a stmt;
      else_body : 'a stmt option;
    }
  | WhileLoop of {
      cond : 'a expr;
      body : 'a stmt;
    }
  | ForLoop of {
      init : 'a stmt;
      cond : 'a expr;
      incr : 'a expr;
      body : 'a stmt;
    }

let precedence = function
  | Or -> 10
  | And -> 20
  | Equal | Neq -> 30
  | Less | Leq | Greater | Geq -> 40
  | BitOr -> 50
  | BitXor -> 60
  | BitAnd -> 70
  | LShift | RShift -> 75
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
  | LShift -> "<<"
  | RShift -> ">>"

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
  | "<<" -> Some LShift
  | ">>" -> Some RShift
  | _ -> None [@coverage off]

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"
  | Compl -> "~"
