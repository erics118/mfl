exception Lex_error of string

type token =
  | Eof
  | Lparen
  | Rparen
  | Integer of int
  | Bool of bool
  | BinaryOp of string
  | UnaryOp of string
  | Semicolon
  | LBrace
  | RBrace

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

let rec skip_whitespace st =
  match peek st with
  | Some (' ' | '\t' | '\r' | '\n') ->
      advance st;
      skip_whitespace st
  | _ -> ()

let read_number st =
  let start = st.pos in
  while
    match peek st with
    | Some c when is_digit c -> true
    | _ -> false
  do
    advance st
  done;
  let literal = String.sub st.input start (st.pos - start) in
  Integer (int_of_string literal)

let read_ident st =
  let start = st.pos in
  while
    match peek st with
    | Some c when is_alpha c || is_digit c -> true
    | _ -> false
  do
    advance st
  done;
  match String.sub st.input start (st.pos - start) with
  | "true" -> Bool true
  | "false" -> Bool false
  | s -> raise (Lex_error (Printf.sprintf "unknown identifier '%s'" s))

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

let string_of_token = function
  | Eof -> "EOF"
  | Semicolon -> ";"
  | LBrace -> "{"
  | RBrace -> "}"
  | Lparen -> "("
  | Rparen -> ")"
  | Integer x -> string_of_int x
  | Bool x -> string_of_bool x
  | BinaryOp x -> x
  | UnaryOp x -> x
