open Token

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
      Integer (int_of_string literal)

let read_ident st =
  let start = st.pos in
  advance_while (fun c -> is_alpha c || is_digit c) st;
  match String.sub st.input start (st.pos - start) with
  | "true" -> Bool true
  | "false" -> Bool false
  | "int" -> IntKw
  | "bool" -> BoolKw
  | "return" -> ReturnKw
  | s -> Identifier s

let peek2 st =
  let pos = st.pos + 1 in
  if pos < String.length st.input then Some st.input.[pos] else None

let gettok st =
  skip_whitespace st;
  match peek st with
  | None -> Eof
  | Some ';' ->
      advance st;
      Semicolon
  | Some ',' ->
      advance st;
      Comma
  | Some '?' ->
      advance st;
      QuestionMark
  | Some ':' ->
      advance st;
      Colon
  | Some '{' ->
      advance st;
      LBrace
  | Some '}' ->
      advance st;
      RBrace
  | Some '(' ->
      advance st;
      Lparen
  | Some ')' ->
      advance st;
      Rparen
  | Some '=' when peek2 st = Some '=' ->
      advance st;
      advance st;
      BinaryOp "=="
  | Some '=' ->
      advance st;
      Assign
  | Some '!' when peek2 st = Some '=' ->
      advance st;
      advance st;
      BinaryOp "!="
  | Some '<' when peek2 st = Some '=' ->
      advance st;
      advance st;
      BinaryOp "<="
  | Some '>' when peek2 st = Some '=' ->
      advance st;
      advance st;
      BinaryOp ">="
  | Some '&' when peek2 st = Some '&' ->
      advance st;
      advance st;
      BinaryOp "&&"
  | Some '|' when peek2 st = Some '|' ->
      advance st;
      advance st;
      BinaryOp "||"
  | Some '!' ->
      advance st;
      UnaryOp "!"
  | Some (('+' | '-' | '*' | '/' | '%' | '<' | '>' | '&' | '|' | '^') as op) ->
      advance st;
      BinaryOp (String.make 1 op)
  | Some c when is_digit c -> read_number st
  | Some c when is_alpha c -> read_ident st
  | Some c -> raise (Lex_error (Printf.sprintf "unexpected character '%c'" c))

let peek_next_token st =
  let saved_pos = st.pos in
  let tok = gettok st in
  st.pos <- saved_pos;
  tok
