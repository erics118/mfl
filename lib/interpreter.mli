(** interpreter *)

(** raised on invalid operand types *)
exception Type_error of string

(** raised on division or modulo by zero *)
exception Div_by_zero

(** [interpret e] evaluates [e] and returns the resulting value expression
    @raise Type_error if operand types are invalid
    @raise Div_by_zero on division by zero *)
val interpret : Ast.expr -> Ast.expr
