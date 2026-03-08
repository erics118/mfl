type token =
  | Eof
  | Lparen
  | Rparen
  | Integer of int
  | BinaryOp of char

type state = {
  input : string;
  mutable pos : int;
}

let create input = { input; pos = 0 }
let has_more st = st.pos < String.length st.input
let peek st = if has_more st then Some st.input.[st.pos] else None
let advance st = st.pos <- st.pos + 1
let is_digit c = c >= '0' && c <= '9'

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
  try Integer (int_of_string literal)
  with Failure _ -> failwith (Printf.sprintf "invalid integer '%s'" literal)

let gettok st =
  skip_whitespace st;
  match peek st with
  | None -> Eof
  | Some '(' ->
      advance st;
      Lparen
  | Some ')' ->
      advance st;
      Rparen
  | Some (('+' | '-' | '*' | '/') as op) ->
      advance st;
      BinaryOp op
  | Some c when is_digit c -> read_number st
  | Some c -> failwith (Printf.sprintf "unexpected character '%c'" c)

let string_of_token = function
  | Eof -> "eof"
  | Lparen -> "("
  | Rparen -> ")"
  | Integer x -> "Integer(" ^ string_of_int x ^ ")"
  | BinaryOp x -> "Integer(" ^ String.make 1 x ^ ")"
