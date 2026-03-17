open Token

exception Lex_error of string

type state = {
  input : string;
  mutable pos : int;
}

let create input = { input; pos = 0 }
let has_more st = st.pos < String.length st.input
let peek st = if has_more st then Some st.input.[st.pos] else None
let advance st = st.pos <- st.pos + 1
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
        (Lex_error (Printf.sprintf "invalid numeric literal '%s'" bad_literal))
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

let next_token st =
  skip_whitespace st;
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
      TokBinaryOp "=="
  | Some '=' ->
      advance st;
      TokAssign
  | Some '!' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokBinaryOp "!="
  | Some '<' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokBinaryOp "<="
  | Some '>' when peek2 st = Some '=' ->
      advance st;
      advance st;
      TokBinaryOp ">="
  | Some '&' when peek2 st = Some '&' ->
      advance st;
      advance st;
      TokBinaryOp "&&"
  | Some '|' when peek2 st = Some '|' ->
      advance st;
      advance st;
      TokBinaryOp "||"
  | Some '!' ->
      advance st;
      TokUnaryOp "!"
  | Some (('+' | '-' | '*' | '/' | '%' | '<' | '>' | '&' | '|' | '^') as op) ->
      advance st;
      TokBinaryOp (String.make 1 op)
  | Some c when is_digit c -> read_number st
  | Some c when is_alpha c -> read_ident st
  | Some c -> raise (Lex_error (Printf.sprintf "unexpected character '%c'" c))

let peek_token st =
  let saved_pos = st.pos in
  let tok = next_token st in
  st.pos <- saved_pos;
  tok
