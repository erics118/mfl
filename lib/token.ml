(* tokens for the ast *)

(** lexical tokens *)
type token =
  (* literals *)
  | TokInt of int * Ast.int_suffix  (** integer literal *)
  | TokFloat of float  (** float literal *)
  | TokDouble of float  (** double literal *)
  | TokLongDouble of float  (** long double literal *)
  | TokBool of bool  (** boolean literal *)
  | TokChar of int  (** character literal, stored as int *)
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
  | TokBreakKw  (** [break] keyword *)
  | TokContinueKw  (** [continue] keyword *)
  | TokDoKw  (** [do] keyword *)
  | TokCharKw  (** [char] keyword *)
  | TokShortKw  (** [short] keyword *)
  | TokLongKw  (** [long] keyword *)
  | TokUnsignedKw  (** [unsigned] keyword *)
  | TokSignedKw  (** [signed] keyword *)
  | TokSizeofKw  (** [sizeof] keyword *)
  | TokTypedefKw  (** [typedef] keyword *)
  | TokStructKw  (** [struct] keyword *)
  | TokFloatKw  (** [float] keyword *)
  | TokDoubleKw  (** [double] keyword *)
  (* binary operators *)
  | TokPlus  (** [+] *)
  | TokMinus  (** [-] *)
  | TokStar  (** [*] *)
  | TokSlash  (** [/] *)
  | TokPercent  (** [%] *)
  | TokEqEq  (** [==] *)
  | TokBangEq  (** [!=] *)
  | TokLt  (** [<] *)
  | TokLtEq  (** [<=] *)
  | TokGt  (** [>] *)
  | TokGtEq  (** [>=] *)
  | TokAmpAmp  (** [&&] *)
  | TokPipePipe  (** [||] *)
  | TokAmp  (** [&] *)
  | TokPipe  (** [|] *)
  | TokCaret  (** [^] *)
  | TokLtLt  (** [<<] *)
  | TokGtGt  (** [>>] *)
  (* unary-only operators *)
  | TokBang  (** [!] *)
  | TokTilde  (** [~] *)
  | TokPlusPlus  (** [++] *)
  | TokMinusMinus  (** [--] *)
  (* assignment *)
  | TokAssign  (** [=] *)
  (* punctuation *)
  | TokLParen  (** [ ( ] *)
  | TokRParen  (** [ ) ] *)
  | TokLBrace  (** [{] *)
  | TokRBrace  (** [}] *)
  | TokLBracket
  | TokRBracket
  | TokSemicolon  (** [;] *)
  | TokComma  (** [,] *)
  | TokQuestion  (** [?] for the ternary operator *)
  | TokColon  (** [:] for the ternary operator*)
  | TokDot  (** [.] member access *)
  | TokArrow  (** [->] pointer member access *)
  (* special *)
  | TokEof  (** end of input *)

(** render a token as a string *)
let string_of_token = function
  (* literals *)
  | TokInt (x, suffix) -> string_of_int x ^ Ast.string_of_int_suffix suffix
  | TokFloat f -> Printf.sprintf "%gf" f
  | TokDouble f -> Printf.sprintf "%g" f
  | TokLongDouble f -> Printf.sprintf "%gL" f
  | TokBool x -> string_of_bool x
  | TokChar c -> Printf.sprintf "'%c'" (Char.chr c)
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
  | TokBreakKw -> "break"
  | TokContinueKw -> "continue"
  | TokDoKw -> "do"
  | TokCharKw -> "char"
  | TokShortKw -> "short"
  | TokLongKw -> "long"
  | TokUnsignedKw -> "unsigned"
  | TokSignedKw -> "signed"
  | TokSizeofKw -> "sizeof"
  | TokTypedefKw -> "typedef"
  | TokStructKw -> "struct"
  | TokFloatKw -> "float"
  | TokDoubleKw -> "double"
  (* binary operators *)
  | TokPlus -> "+"
  | TokMinus -> "-"
  | TokStar -> "*"
  | TokSlash -> "/"
  | TokPercent -> "%"
  | TokEqEq -> "=="
  | TokBangEq -> "!="
  | TokLt -> "<"
  | TokLtEq -> "<="
  | TokGt -> ">"
  | TokGtEq -> ">="
  | TokAmpAmp -> "&&"
  | TokPipePipe -> "||"
  | TokAmp -> "&"
  | TokPipe -> "|"
  | TokCaret -> "^"
  | TokLtLt -> "<<"
  | TokGtGt -> ">>"
  (* unary-only operators *)
  | TokBang -> "!"
  | TokTilde -> "~"
  | TokPlusPlus -> "++"
  | TokMinusMinus -> "--"
  (* assignment *)
  | TokAssign -> "="
  (* punctuation *)
  | TokLParen -> "("
  | TokRParen -> ")"
  | TokLBrace -> "{"
  | TokRBrace -> "}"
  | TokLBracket -> "["
  | TokRBracket -> "]"
  | TokSemicolon -> ";"
  | TokComma -> ","
  | TokQuestion -> "?"
  | TokColon -> ":"
  | TokDot -> "."
  | TokArrow -> "->"
  (* special *)
  | TokEof -> "EOF"
