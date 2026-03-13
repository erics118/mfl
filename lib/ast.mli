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

(** unary operators *)
type uop =
  | Neg  (** numeric negation *)
  | Not  (** logical negation *)

(** variable and return types *)
type var_type = VarType of string

(** render a variable type as a string *)
val string_of_var_type : var_type -> string

(** expressions and statements *)
type expr =
  | IntLiteral of int  (** integer literal *)
  | BoolLiteral of bool  (** boolean literal *)
  | VarRef of string  (** variable reference *)
  | BinaryOp of op * expr * expr
      (** [expr1 op expr2] is a binary operator application *)
  | UnaryOp of uop * expr  (** unary operator application *)
  | Statement of expr  (** expression statement *)
  | ReturnStmt of expr option
      (** return statement, ie [return expr;] or [return;] *)
  | EmptyStmt  (** empty statement, ie [;] *)
  | CompoundStmt of expr list
      (** block of statements, ie [stmt; stmt; stmt;] *)
  | Ternary of expr * expr * expr
      (** ternary operator, ie [expr1 ? expr2 : expr3] *)
  | VarDef of {
      var_type : var_type;
      name : string;
      init : expr;
    }  (** variable definition with initializer, ie [var_type name = init;] *)
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : expr list;
    }  (** function definition, ie [ret_type name(params...) { body }] *)
  | FuncCall of {
      name : string;
      args : expr list;
    }  (** function call, ie [name(args...)] *)

(** [precedence op] returns the binding precedence of [op] higher numbers bind
    more tightly *)
val precedence : op -> int

(** [string_of_op] renders a binary operator into its lexeme *)
val string_of_op : op -> string

(** [op_of_string_opt str] parses a binary operator lexeme into [Some(op)] or
    [None] if invalid *)
val op_of_string_opt : string -> op option

(** [string_of_uop uop] renders a unary operator into its lexeme *)
val string_of_uop : uop -> string
