(* tokens for the ast *)

(** lexical tokens *)
type token =
  (* literals *)
  | TokInt of int  (** integer literal *)
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
  (* special *)
  | TokEof  (** end of input *)

(** render a token as a string *)
let string_of_token = function
  (* literals *)
  | TokInt x -> string_of_int x
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
  (* special *)
  | TokEof -> "EOF"
