(** lexer state *)

type state

(** raised on invalid input *)
exception Lex_error of string

(** [create input] creates a lexer state for [input] *)
val create : string -> state

(** [gettok st] returns the next token and advances [st]
    @raise Lex_error on invalid input *)
val gettok : state -> Token.token

(** [peek_next_token st] returns the next token without consuming it
    @raise Lex_error on invalid input *)
val peek_next_token : state -> Token.token
