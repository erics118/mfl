(** interpreter *)

(** raised on invalid operand types *)
exception Type_error of Ast.pos * string

(** raised on division or modulo by zero *)
exception Div_by_zero of Ast.pos

(** [interpret s] evaluates statement [s] and returns the last produced value,
    or [None] if no value was produced.
    @raise Type_error if operand types are invalid
    @raise Div_by_zero on division by zero *)
val interpret : Ast.parsed Ast.stmt -> Ast.parsed Ast.expr option
