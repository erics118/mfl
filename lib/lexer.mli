(** lexer state *)

type state

(** raised on invalid input *)
exception Lex_error of Ast.pos * string

(** [create input] creates a lexer state for [input] *)
val create : string -> state

(** [tok_pos st] returns the source position of the most recently returned
    token, after whitespace was skipped *)
val tok_pos : state -> Ast.pos

(** [next_token st] returns the next token and advances [st]
    @raise Lex_error on invalid input *)
val next_token : state -> Token.token

(** [peek_token st] returns the next token without consuming it
    @raise Lex_error on invalid input *)
val peek_token : state -> Token.token
