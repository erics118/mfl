(** lexer state *)

open Token
include Lexer_state
include Lexer_reader

(** [next_token st] returns the next token and advances [st]
    @raise Lex_error on invalid input *)
let next_token st =
  skip_whitespace_and_comments st;
  (* snapshot position at start of the token being returned *)
  st.tok_line <- st.line;
  st.tok_col <- st.col;
  match peek st with
  | None -> TokEof
  | Some ';' ->
      advance st;
      TokSemicolon
  | Some ',' ->
      advance st;
      TokComma
  | Some '?' ->
      advance st;
      TokQuestion
  | Some ':' ->
      advance st;
      TokColon
  | Some '{' ->
      advance st;
      TokLBrace
  | Some '}' ->
      advance st;
      TokRBrace
  | Some '(' ->
      advance st;
      TokLParen
  | Some ')' ->
      advance st;
      TokRParen
  | Some '[' ->
      advance st;
      TokLBracket
  | Some ']' ->
      advance st;
      TokRBracket
  | Some '=' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokEqEq
  | Some '=' ->
      advance st;
      TokAssign
  | Some '!' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokBangEq
  | Some '<' when peek2 st = Some '<' ->
      advance st;
      advance st;
      TokLtLt
  | Some '<' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokLtEq
  | Some '>' when peek2 st = Some '>' ->
      advance st;
      advance st;
      TokGtGt
  | Some '>' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokGtEq
  | Some '&' when peek2 st = Some '&' ->
      advance st;
      advance st;
      TokAmpAmp
  | Some '|' when peek2 st = Some '|' ->
      advance st;
      advance st;
      TokPipePipe
  | Some '!' ->
      advance st;
      TokBang
  | Some '~' ->
      advance st;
      TokTilde
  | Some '+' when peek2 st = Some '+' ->
      advance st;
      advance st;
      TokPlusPlus
  | Some '-' when peek2 st = Some '-' ->
      advance st;
      advance st;
      TokMinusMinus
  | Some '-' when peek2 st = Some '>' ->
      advance st;
      advance st;
      TokArrow
  | Some '+' ->
      advance st;
      TokPlus
  | Some '-' ->
      advance st;
      TokMinus
  | Some '.' when peek2 st = Some '.' && peek3 st = Some '.' ->
      advance st;
      advance st;
      advance st;
      TokEllipsis
  | Some '.' when Option.fold ~none:false ~some:is_digit (peek2 st) ->
      read_dot_number st
  | Some '.' ->
      advance st;
      TokDot
  | Some '*' ->
      advance st;
      TokStar
  | Some '/' ->
      advance st;
      TokSlash
  | Some '%' ->
      advance st;
      TokPercent
  | Some '<' ->
      advance st;
      TokLt
  | Some '>' ->
      advance st;
      TokGt
  | Some '&' ->
      advance st;
      TokAmp
  | Some '|' ->
      advance st;
      TokPipe
  | Some '^' ->
      advance st;
      TokCaret
  | Some '"' -> read_string st
  | Some '\'' -> read_char st
  | Some c when is_digit c -> read_number st
  | Some c when is_alpha c -> read_ident st
  | Some c ->
      raise
        (Lex_error (tok_pos st, Printf.sprintf "unexpected character '%c'" c))

(** [peek_token st] returns the next token without consuming it
    @raise Lex_error on invalid input *)
let peek_token st =
  let saved = snapshot st in
  let tok = next_token st in
  restore st saved;
  tok
