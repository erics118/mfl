(** lexer state *)

open Token

(** raised on invalid input *)
exception Lex_error of Ast.pos * string

type state = {
  input : string;
  mutable pos : int;
  mutable line : int;
  mutable col : int;
  (* position snapshotted at the start of the most-recently-returned token,
     after skipping whitespace *)
  mutable tok_line : int;
  mutable tok_col : int;
}

(** [create input] creates a lexer state for [input] *)
let create input =
  { input; pos = 0; line = 1; col = 1; tok_line = 1; tok_col = 1 }

let has_more st = st.pos < String.length st.input
let peek st = if has_more st then Some st.input.[st.pos] else None

let advance st =
  if has_more st && st.input.[st.pos] = '\n' then (
    st.line <- st.line + 1;
    st.col <- 1)
  else st.col <- st.col + 1;
  st.pos <- st.pos + 1

(** [tok_pos st] returns the source position of the most recently returned
    token, after whitespace was skipped *)
let tok_pos st = Ast.{ line = st.tok_line; col = st.tok_col }

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let advance_while pred st =
  while
    match peek st with
    | Some c when pred c -> true
    | _ -> false
  do
    advance st
  done

let skip_whitespace st =
  advance_while
    (function
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false)
    st

let read_number st =
  let start = st.pos in
  advance_while is_digit st;
  match peek st with
  | Some c when is_alpha c ->
      advance_while (fun c -> is_alpha c || is_digit c) st;
      let bad_literal = String.sub st.input start (st.pos - start) in
      raise
        (Lex_error
           ( tok_pos st,
             Printf.sprintf "invalid numeric literal '%s'" bad_literal ))
  | _ ->
      let literal = String.sub st.input start (st.pos - start) in
      TokInt (int_of_string literal)

let read_ident st =
  let start = st.pos in
  advance_while (fun c -> is_alpha c || is_digit c) st;
  match String.sub st.input start (st.pos - start) with
  | "true" -> TokBool true
  | "false" -> TokBool false
  | "int" -> TokIntKw
  | "bool" -> TokBoolKw
  | "return" -> TokReturnKw
  | "if" -> TokIfKw
  | "else" -> TokElseKw
  | "while" -> TokWhileKw
  | "for" -> TokForKw
  | "void" -> TokVoidKw
  | s -> TokIdent s

let peek2 st =
  let pos = st.pos + 1 in
  if pos < String.length st.input then Some st.input.[pos] else None

(** [next_token st] returns the next token and advances [st]
    @raise Lex_error on invalid input *)
let next_token st =
  skip_whitespace st;
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
  | Some '+' ->
      advance st;
      TokPlus
  | Some '-' ->
      advance st;
      TokMinus
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
  | Some c when is_digit c -> read_number st
  | Some c when is_alpha c -> read_ident st
  | Some c ->
      raise
        (Lex_error (tok_pos st, Printf.sprintf "unexpected character '%c'" c))

(** [peek_token st] returns the next token without consuming it
    @raise Lex_error on invalid input *)
let peek_token st =
  let saved_pos = st.pos in
  let saved_line = st.line in
  let saved_col = st.col in
  let saved_tok_line = st.tok_line in
  let saved_tok_col = st.tok_col in
  let tok = next_token st in
  st.pos <- saved_pos;
  st.line <- saved_line;
  st.col <- saved_col;
  st.tok_line <- saved_tok_line;
  st.tok_col <- saved_tok_col;
  tok
