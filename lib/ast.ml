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
  | AddrOf  (** address-of *)
  | Deref  (** pointer dereference *)

(** variable and return types *)
type var_type =
  | VBool
  | VVoid
  | VChar  (** char, but is implemented as signed char *)
  | VSChar  (** signed char *)
  | VUChar
  | VShort
  | VUShort
  | VInt
  | VUInt
  | VLong
  | VULong
  | VLongLong
  | VULongLong
  | VPtr of var_type
  | VNamed of string  (** user-defined type names *)

(** render a variable type as a string *)
let rec string_of_var_type = function
  | VBool -> "bool"
  | VVoid -> "void"
  | VChar -> "char"
  | VSChar -> "signed char"
  | VUChar -> "unsigned char"
  | VShort -> "short"
  | VUShort -> "unsigned short"
  | VInt -> "int"
  | VUInt -> "unsigned int"
  | VLong -> "long"
  | VULong -> "unsigned long"
  | VLongLong -> "long long"
  | VULongLong -> "unsigned long long"
  | VPtr t -> string_of_var_type t ^ "*"
  | VNamed name -> name

(** source location *)
type pos = {
  line : int;
  col : int;
}

(** types resolved by the typechecker *)
type typ =
  | Void
  | Bool  (** i1 *)
  | Char  (** i8, implementation-defined signedness *)
  | SChar  (** i8, explicitly signed *)
  | UChar  (** i8, unsigned *)
  | Short  (** i16, signed *)
  | UShort  (** i16, unsigned *)
  | Int  (** i32, signed *)
  | UInt  (** i32, unsigned *)
  | Long  (** i64, signed *)
  | ULong  (** i64, unsigned *)
  | LongLong  (** i64, signed *)
  | ULongLong  (** i64, unsigned *)
  | Ptr of typ (* pointer type *)

(** [typ_of_var_type vt] converts a built-in [var_type] to its [typ]. Only safe
    to call after typechecking, when all [VNamed] types have already been
    validated. Raises [Assert_failure] if called with [VNamed]. *)
let rec typ_of_var_type = function
  | VBool -> Bool
  | VVoid -> Void
  | VChar -> Char
  | VSChar -> SChar
  | VUChar -> UChar
  | VShort -> Short
  | VUShort -> UShort
  | VInt -> Int
  | VUInt -> UInt
  | VLong -> Long
  | VULong -> ULong
  | VLongLong -> LongLong
  | VULongLong -> ULongLong
  | VPtr t -> Ptr (typ_of_var_type t)
  | VNamed _ -> assert false [@coverage off]

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
  | CharLiteral : 'a ann * int -> 'a expr  (** stored as int for simplicity *)
  | VarRef : 'a ann * string -> 'a expr
  | BinaryOp : 'a ann * op * 'a expr * 'a expr -> 'a expr
  | UnaryOp : 'a ann * uop * 'a expr -> 'a expr
  | Ternary : 'a ann * 'a expr * 'a expr * 'a expr -> 'a expr
  | FuncCall : 'a ann * string * 'a expr list -> 'a expr
  | Assign : 'a ann * 'a expr * 'a expr -> 'a expr
  | PreInc : 'a ann * 'a expr -> 'a expr
  | PreDec : 'a ann * 'a expr -> 'a expr
  | PostInc : 'a ann * 'a expr -> 'a expr
  | PostDec : 'a ann * 'a expr -> 'a expr
  | Cast : 'a ann * var_type * 'a expr -> 'a expr  (** produced by the parser *)
  | ImplicitCast : checked ann * typ * checked expr -> checked expr
      (** only produced by the typechecker *)

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
  | BitOr -> 30
  | BitXor -> 40
  | BitAnd -> 50
  | Equal | Neq -> 60
  | Less | Leq | Greater | Geq -> 70
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
  | AddrOf -> "&"
  | Deref -> "*"
