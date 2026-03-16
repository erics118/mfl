(** interpreter *)

(** raised on invalid operand types *)
exception Type_error of string

(** raised on division or modulo by zero *)
exception Div_by_zero

(** [interpret s] evaluates statement [s] and returns the last produced value,
    or [None] if no value was produced.
    @raise Type_error if operand types are invalid
    @raise Div_by_zero on division by zero *)
val interpret : Ast.stmt -> Ast.expr option
