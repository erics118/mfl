type token =
  | Eof
  | Lparen
  | Rparen
  | Number of float
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
  let rec loop seen_dot =
    match peek st with
    | Some c when is_digit c ->
        advance st;
        loop seen_dot
    | Some '.' when not seen_dot ->
        advance st;
        loop true
    | _ -> ()
  in
  loop false;
  let literal = String.sub st.input start (st.pos - start) in
  try Number (float_of_string literal)
  with Failure _ -> failwith (Printf.sprintf "invalid number '%s'" literal)

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
  | Some c when is_digit c || c = '.' -> read_number st
  | Some c -> failwith (Printf.sprintf "unexpected character '%c'" c)

let string_of_token = function
  | Eof -> "eof"
  | Lparen -> "("
  | Rparen -> ")"
  | Number x -> "Number(" ^ string_of_float x ^ ")"
  | BinaryOp x -> "Number(" ^ String.make 1 x ^ ")"
