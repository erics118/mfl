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
val dummy_pos : pos

(** extract the source position from any annotation *)
val pos_of : 'a ann -> pos

(** extract the resolved type from a checked annotation *)
val typ_of : checked ann -> typ

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

(** statements *)
type stmt =
  | ExprStmt of parsed expr  (** [expr;] is a single expression *)
  | ReturnStmt of parsed expr option
      (** [return expr;] or [return;] returns a value from a function *)
  | EmptyStmt  (** [;] is an empty statement *)
  | CompoundStmt of stmt list
      (** sequence of statements surrounded by braces *)
  | VarDef of {
      var_type : var_type;
      name : string;
      init : parsed expr;
    }  (** [var_type name = init;] defines a variable with an initial value *)
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : stmt list;
    }  (** [ret_type name(params) { body }] defines a function *)
  | If of {
      cond : parsed expr;
      then_body : stmt;
      else_body : stmt option;
    }
      (** [if (cond) if_body] or [if (cond) if_body else else_body] is an
          if-else statement *)
  | WhileLoop of {
      cond : parsed expr;
      body : stmt;
    }  (** [while (cond) body] is a while loop *)
  | ForLoop of {
      init : stmt;
      cond : parsed expr;
      incr : parsed expr;
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
