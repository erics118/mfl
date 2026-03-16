type token =
  | Eof
  | LParen
  | RParen
  | Integer of int
  | Bool of bool
  | IntKw
  | BoolKw
  | Identifier of string
  | BinaryOp of string
  | UnaryOp of string
  | Assign
  | Semicolon
  | Comma
  | ReturnKw
  | IfKw
  | ElseKw
  | WhileKw
  | ForKw
  | LBrace
  | RBrace
  | QuestionMark
  | Colon

let string_of_token = function
  | Eof -> "EOF"
  | Semicolon -> ";"
  | Comma -> ","
  | QuestionMark -> "?"
  | Colon -> ":"
  | LBrace -> "{"
  | RBrace -> "}"
  | LParen -> "("
  | RParen -> ")"
  | Integer x -> string_of_int x
  | Bool x -> string_of_bool x
  | IntKw -> "int"
  | BoolKw -> "bool"
  | Identifier x -> x
  | BinaryOp x -> x
  | UnaryOp x -> x
  | Assign -> "="
  | ReturnKw -> "return"
  | IfKw -> "if"
  | ElseKw -> "else"
  | WhileKw -> "while"
  | ForKw -> "for"
