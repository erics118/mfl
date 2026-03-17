type token =
  (* literals *)
  | TokInt of int
  | TokBool of bool
  (* identifiers *)
  | TokIdent of string
  (* keywords *)
  | TokIntKw
  | TokBoolKw
  | TokVoidKw
  | TokReturnKw
  | TokIfKw
  | TokElseKw
  | TokWhileKw
  | TokForKw
  (* operators *)
  | TokBinaryOp of string
  | TokUnaryOp of string
  | TokAssign
  (* punctuation *)
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokSemicolon
  | TokComma
  | TokQuestion
  | TokColon
  (* special *)
  | TokEof

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
