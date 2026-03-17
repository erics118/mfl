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
val string_of_var_type : var_type -> string

(** expressions *)
type expr =
  | IntLiteral of int  (** integer literal *)
  | BoolLiteral of bool  (** boolean literal *)
  | VarRef of string  (** variable reference *)
  | BinaryOp of op * expr * expr
      (** [expr1 op expr2] is a binary operator application *)
  | UnaryOp of uop * expr  (** [op expr] is a unary operator application, *)
  | Ternary of expr * expr * expr
      (** [expr1 ? expr2 : expr3] is the ternary operator *)
  | FuncCall of {
      name : string;
      args : expr list;
    }  (** [name(args)] is a function call *)
  | Assign of {
      name : string;
      value : expr;
    }  (** [name = value] assigns a value to an existing variable *)

(** statements *)
type stmt =
  | ExprStmt of expr  (** [expr;] is a single expression *)
  | ReturnStmt of expr option
      (** [return expr;] or [return;] returns a value from a function *)
  | EmptyStmt  (** [;] is an empty statement *)
  | CompoundStmt of stmt list
      (** sequence of statements surrounded by braces *)
  | VarDef of {
      var_type : var_type;
      name : string;
      init : expr;
    }  (** [var_type name = init;] defines a variable with an initial value *)
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : stmt list;
    }  (** [ret_type name(params) { body }] defines a function *)
  | If of {
      cond : expr;
      then_body : stmt;
      else_body : stmt option;
    }
      (** [if (cond) if_body] or [if (cond) if_body else else_body] is an
          if-else statement *)
  | WhileLoop of {
      cond : expr;
      body : stmt;
    }  (** [while (cond) body] is a while loop *)
  | ForLoop of {
      init : stmt;
      cond : expr;
      incr : expr;
      body : stmt;
    }  (** [for (init; cond; incr) body] is a for loop *)

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
