open Token
open Lexer_state

let rec skip_whitespace_and_comments st =
  (* skip whitespace *)
  advance_while
    (function
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false)
    st;
  (* line comment, skip until end of line *)
  if peek st = Some '/' && peek2 st = Some '/' then begin
    advance_while (fun c -> c <> '\n') st;
    skip_whitespace_and_comments st
  end (* block comment, skip until close *)
  else if peek st = Some '/' && peek2 st = Some '*' then begin
    advance st;
    advance st;
    let rec skip () =
      match peek st with
      | None ->
          (* reached end of file, error *)
          raise (Lex_error (tok_pos st, "unterminated block comment"))
      | Some '*' when peek2 st = Some '/' ->
          (* stop if is comment terminator *)
          advance st;
          advance st
      | _ ->
          (* keep advancing *)
          advance st;
          skip ()
    in
    skip ();
    (* skip more whitespace as necessary *)
    skip_whitespace_and_comments st
  end

let read_number st =
  let start = st.pos in
  advance_while is_digit st;
  match peek st with
  | Some c when is_alpha c ->
      advance_while is_alnum st;
      let bad_literal = String.sub st.input start (st.pos - start) in
      raise
        (Lex_error
           ( tok_pos st,
             Printf.sprintf "invalid numeric literal '%s'" bad_literal ))
  | _ ->
      let literal = String.sub st.input start (st.pos - start) in
      TokInt (int_of_string literal)

let read_hex_escape_sequence st =
  (* hex escape sequence *)
  let start = st.pos in
  advance_while is_hex st;
  let digits = String.sub st.input start (st.pos - start) in
  if String.length digits = 0 then
    raise (Lex_error (tok_pos st, "empty hex escape sequence"));
  let hex_val = int_of_string ("0x" ^ digits) in
  if hex_val > 255 then
    raise (Lex_error (tok_pos st, "hex escape out of range"));
  hex_val

let read_escape_sequence st =
  advance st;
  (* handle escape sequence *)
  let peeked = peek st in
  advance st;
  match peeked with
  | Some '0' -> 0
  | Some 'a' -> 7
  | Some 'b' -> 8
  | Some 't' -> 9
  | Some 'n' -> 10
  | Some 'v' -> 11
  | Some 'f' -> 12
  | Some 'r' -> 13
  | Some '\\' -> 92
  | Some '\'' -> 39
  | Some 'x' -> read_hex_escape_sequence st
  | _ ->
      let msg =
        match peeked with
        | Some c -> Printf.sprintf "unrecognized escape sequence '\\%c'" c
        | None -> "unrecognized escape sequence at end of input"
      in
      raise (Lex_error (tok_pos st, msg))

let read_char st =
  advance st;
  let value =
    match peek st with
    | Some '\\' -> read_escape_sequence st
    | Some c when c <> '\'' && c <> '\n' && c <> '\r' && c <> '\000' ->
        (* normal character value *)
        advance st;
        Char.code c
    | _ -> raise (Lex_error (tok_pos st, "invalid character literal"))
  in
  match peek st with
  | Some '\'' ->
      advance st;
      TokChar value
  | _ -> raise (Lex_error (tok_pos st, "expected closing '\''"))

let read_ident st =
  let start = st.pos in
  advance_while is_alnum st;
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
  | "break" -> TokBreakKw
  | "continue" -> TokContinueKw
  | "do" -> TokDoKw
  | "char" -> TokCharKw
  | "short" -> TokShortKw
  | "long" -> TokLongKw
  | "unsigned" -> TokUnsignedKw
  | "signed" -> TokSignedKw
  | "void" -> TokVoidKw
  | s -> TokIdent s
