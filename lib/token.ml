(* tokens for the ast *)

(** lexical tokens *)
type token =
  (* literals *)
  | TokInt of int  (** integer literal *)
  | TokBool of bool  (** boolean literal *)
  (* identifiers *)
  | TokIdent of string  (** identifier *)
  (* keywords *)
  | TokIntKw  (** [int] keyword *)
  | TokBoolKw  (** [bool] keyword *)
  | TokVoidKw  (** [void] keyword *)
  | TokReturnKw  (** [return] keyword *)
  | TokIfKw  (** [if] keyword *)
  | TokElseKw  (** [else] keyword *)
  | TokWhileKw  (** [while] keyword *)
  | TokForKw  (** [for] keyword *)
  (* operators *)
  | TokBinaryOp of string  (** binary operator lexeme *)
  | TokUnaryOp of string  (** unary operator lexeme *)
  | TokAssign  (** [=] *)
  (* punctuation *)
  | TokLParen  (** [ ( ] *)
  | TokRParen  (** [ ) ] *)
  | TokLBrace  (** [{] *)
  | TokRBrace  (** [}] *)
  | TokSemicolon  (** [;] *)
  | TokComma  (** [,] *)
  | TokQuestion  (** [?] for the ternary operator *)
  | TokColon  (** [:] for the ternary operator*)
  (* special *)
  | TokEof  (** end of input *)

(** render a token as a string *)
let string_of_token = function
  (* literals *)
  | TokInt x -> string_of_int x
  | TokBool x -> string_of_bool x
  (* identifiers *)
  | TokIdent x -> x
  (* keywords *)
  | TokIntKw -> "int"
  | TokBoolKw -> "bool"
  | TokVoidKw -> "void"
  | TokReturnKw -> "return"
  | TokIfKw -> "if"
  | TokElseKw -> "else"
  | TokWhileKw -> "while"
  | TokForKw -> "for"
  (* operators *)
  | TokBinaryOp x -> x
  | TokUnaryOp x -> x
  | TokAssign -> "="
  (* punctuation *)
  | TokLParen -> "("
  | TokRParen -> ")"
  | TokLBrace -> "{"
  | TokRBrace -> "}"
  | TokSemicolon -> ";"
  | TokComma -> ","
  | TokQuestion -> "?"
  | TokColon -> ":"
  (* special *)
  | TokEof -> "EOF"
