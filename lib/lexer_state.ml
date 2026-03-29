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
let is_alnum c = is_digit c || is_alpha c
let is_hex c = is_digit c || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')

let advance_while pred st =
  while
    match peek st with
    | Some c when pred c -> true
    | _ -> false
  do
    advance st
  done

let peek2 st =
  let pos = st.pos + 1 in
  if pos < String.length st.input then Some st.input.[pos] else None

type snapshot = {
  snap_pos : int;
  snap_line : int;
  snap_col : int;
  snap_tok_line : int;
  snap_tok_col : int;
}

let snapshot (st : state) =
  {
    snap_pos = st.pos;
    snap_line = st.line;
    snap_col = st.col;
    snap_tok_line = st.tok_line;
    snap_tok_col = st.tok_col;
  }

let restore (st : state) (snap : snapshot) =
  st.pos <- snap.snap_pos;
  st.line <- snap.snap_line;
  st.col <- snap.snap_col;
  st.tok_line <- snap.snap_tok_line;
  st.tok_col <- snap.snap_tok_col
