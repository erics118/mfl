(** abstract syntax tree types and operator utilities *)

(** binary operators *)
type op =
  | Add  (** [ + ] *)
  | Sub  (** [ - ]*)
  | Mul  (** [ * ] *)
  | Div  (** [ / ] *)
  | Mod  (** [ % ] *)
  | Equal  (** [ == ] *)
  | Neq  (** [ != ] *)
  | Less  (** [ < ] *)
  | Leq  (** [ <= ] *)
  | Greater  (** [ > ] *)
  | Geq  (** [ >= ] *)
  | And  (** [ && ] *)
  | Or  (** [ || ] *)
  | BitAnd  (** [ & ] *)
  | BitOr  (** [ | ] *)
  | BitXor  (** [ ^ ] *)
  | LShift  (** [ << ] *)
  | RShift  (** [ >> ] *)

(** unary operators *)
type uop =
  | Neg  (** numeric negation *)
  | Not  (** logical negation *)
  | Compl  (** bitwise complement *)

(** variable and return types *)
type var_type = VarType of string

(** render a variable type as a string *)
let string_of_var_type (VarType name) = name

(** source location *)
type pos = {
  line : int;
  col : int;
}

(** types resolved by the typechecker *)
type typ =
  | Int
  | Bool
  | Void

(** phantom types marking which compiler phase produced an expr *)
type parsed

type checked

(** annotation on every expr node; [Parsed] holds position only, [Checked] adds
    the resolved type *)
type _ ann =
  | Parsed : pos -> parsed ann
  | Checked : pos * typ -> checked ann

(** placeholder position used by the parser until position tracking is added *)
let dummy_pos = { line = 0; col = 0 }

(** extract the source position from any annotation *)
let pos_of : type a. a ann -> pos = function
  | Parsed p -> p
  | Checked (p, _) -> p

(** extract the resolved type from a checked annotation *)
let typ_of : checked ann -> typ = function
  | Checked (_, t) -> t

(** expressions *)
type 'a expr =
  | IntLiteral : 'a ann * int -> 'a expr
  | BoolLiteral : 'a ann * bool -> 'a expr
  | VarRef : 'a ann * string -> 'a expr
  | BinaryOp : 'a ann * op * 'a expr * 'a expr -> 'a expr
  | UnaryOp : 'a ann * uop * 'a expr -> 'a expr
  | Ternary : 'a ann * 'a expr * 'a expr * 'a expr -> 'a expr
  | FuncCall : 'a ann * string * 'a expr list -> 'a expr
  | Assign : 'a ann * string * 'a expr -> 'a expr
  | PreInc : 'a ann * 'a expr -> 'a expr
  | PreDec : 'a ann * 'a expr -> 'a expr
  | PostInc : 'a ann * 'a expr -> 'a expr
  | PostDec : 'a ann * 'a expr -> 'a expr

(** statements *)
type 'a stmt =
  | ExprStmt of pos * 'a expr  (** [expr;] is a single expression *)
  | ReturnStmt of pos * 'a expr option
      (** [return expr;] or [return;] returns a value from a function *)
  | EmptyStmt of pos  (** [;] is an empty statement *)
  | CompoundStmt of pos * 'a stmt list
      (** sequence of statements surrounded by braces *)
  | VarDef of {
      pos : pos;
      var_type : var_type;
      name : string;
      init : 'a expr option;
    }  (** [var_type name = init;] defines a variable with an initial value *)
  | FuncDef of {
      pos : pos;
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : 'a stmt list;
    }  (** [ret_type name(params) { body }] defines a function *)
  | If of {
      pos : pos;
      cond : 'a expr;
      then_body : 'a stmt;
      else_body : 'a stmt option;
    }
      (** [if (cond) if_body] or [if (cond) if_body else else_body] is an
          if-else statement *)
  | BreakStmt of pos  (** [break;] exits the innermost loop *)
  | ContinueStmt of pos
      (** [continue;] jumps to the next iteration of the innermost loop *)
  | WhileLoop of {
      pos : pos;
      cond : 'a expr;
      body : 'a stmt;
    }  (** [while (cond) body] is a while loop *)
  | ForLoop of {
      pos : pos;
      init : 'a stmt;
      cond : 'a expr option;
      incr : 'a expr option;
      body : 'a stmt;
    }  (** [for (init; cond; incr) body] is a for loop *)
  | DoWhileLoop of {
      pos : pos;
      body : 'a stmt;
      cond : 'a expr;
    }  (** [do body while (cond);] is a do-while loop *)

(** [precedence op] returns the binding precedence of [op] higher numbers bind
    more tightly *)
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

(** [string_of_op] renders a binary operator into its lexeme *)
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

(** [op_of_string_opt str] parses a binary operator lexeme into [Some(op)] or
    [None] if invalid *)
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

(** [string_of_uop uop] renders a unary operator into its lexeme *)
let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"
  | Compl -> "~"
