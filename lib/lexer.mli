(** lexer state *)

type state

(** raised on invalid input *)
exception Lex_error of string

(** [create input] creates a lexer state for [input] *)
val create : string -> state

(** [next_token st] returns the next token and advances [st]
    @raise Lex_error on invalid input *)
val next_token : state -> Token.token

(** [peek_token st] returns the next token without consuming it
    @raise Lex_error on invalid input *)
val peek_token : state -> Token.token
