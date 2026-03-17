(** parser entrypoint *)

(** raised on parse errors *)
exception Parse_error of Ast.pos * string

(** [parse input] parses [input] into an ast
    @raise Parse_error if the input is malformed *)
val parse : string -> Ast.stmt
