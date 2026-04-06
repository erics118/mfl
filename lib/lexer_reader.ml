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

(** finish reading a decimal float literal, the part after the dot. handles [f]
    and [l] suffixes *)
let finish_decimal_float_literal st start =
  let invalid_literal () =
    advance_while is_alnum st;
    let literal = String.sub st.input start (st.pos - start) in
    raise
      (Lex_error
         (tok_pos st, Printf.sprintf "invalid numeric literal '%s'" literal))
  in
  let read_literal () = String.sub st.input start (st.pos - start) in
  let suffix =
    match peek st with
    | Some ('f' | 'F') ->
        advance st;
        `Float
    | Some ('l' | 'L') ->
        advance st;
        `LongDouble
    | _ -> `Double
  in
  (* fail if detects alphabetic character *)
  begin match peek st with
  | Some c when is_alpha c -> invalid_literal ()
  | _ -> ()
  end;
  let literal = read_literal () in
  let len = String.length literal in
  (* remove a trailing float/long-double suffix *)
  let clean =
    match suffix with
    | `Double -> literal
    | `Float | `LongDouble -> String.sub literal 0 (len - 1)
  in
  match suffix with
  | `Float -> TokFloat (float_of_string clean)
  | `Double -> TokDouble (float_of_string clean)
  | `LongDouble -> TokLongDouble (float_of_string clean)

(** read a number: either an integer or a float/double *)
let read_number st =
  let start = st.pos in
  let invalid_literal () =
    advance_while is_alnum st;
    let literal = String.sub st.input start (st.pos - start) in
    raise
      (Lex_error
         (tok_pos st, Printf.sprintf "invalid numeric literal '%s'" literal))
  in
  let read_int_suffix () =
    match (peek st, peek2 st) with
    (* ul/ull *)
    | Some ('u' | 'U'), Some ('l' | 'L') ->
        advance st;
        advance st;
        begin match peek st with
        | Some ('l' | 'L') ->
            advance st;
            Ast.UnsignedLongLongSuffix
        | _ -> Ast.UnsignedLongSuffix
        end
    (* lu *)
    | Some ('l' | 'L'), Some ('u' | 'U') ->
        advance st;
        advance st;
        Ast.UnsignedLongSuffix
    (* ll/llu *)
    | Some ('l' | 'L'), Some ('l' | 'L') ->
        advance st;
        advance st;
        begin match peek st with
        | Some ('u' | 'U') ->
            advance st;
            Ast.UnsignedLongLongSuffix
        | _ -> Ast.LongLongSuffix
        end
    (* u *)
    | Some ('u' | 'U'), _ ->
        advance st;
        UnsignedSuffix
    (* l *)
    | Some ('l' | 'L'), _ ->
        advance st;
        begin match peek st with
        | Some ('u' | 'U') ->
            advance st;
            Ast.UnsignedLongSuffix
        | _ -> Ast.LongSuffix
        end
    | _ -> Ast.NoIntSuffix
  in
  advance_while is_digit st;
  let digits_end = st.pos in
  if peek st = Some '.' then begin
    advance st;
    advance_while is_digit st;
    finish_decimal_float_literal st start
  end
  else begin
    let suffix = read_int_suffix () in
    match peek st with
    | Some c when is_alpha c -> invalid_literal ()
    | _ ->
        let digits = String.sub st.input start (digits_end - start) in
        TokInt (int_of_string digits, suffix)
  end

(** read a complete number starting with a dot *)
let read_dot_number st =
  let start = st.pos in
  advance st;
  advance_while is_digit st;
  finish_decimal_float_literal st start

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
  | Some '"' -> 34
  | Some '\'' -> 39
  | Some 'x' -> read_hex_escape_sequence st
  | _ ->
      let msg =
        match peeked with
        | Some c -> Printf.sprintf "unrecognized escape sequence '\\%c'" c
        | None -> "unrecognized escape sequence at end of input"
      in
      raise (Lex_error (tok_pos st, msg))

let read_string st =
  let bytes = ref [] in
  advance st;
  let rec loop () =
    match peek st with
    (* end of string *)
    | Some '"' ->
        advance st;
        TokString (List.rev !bytes)
    (* escape sequence *)
    | Some '\\' ->
        let value = read_escape_sequence st in
        bytes := value :: !bytes;
        loop ()
    (* bad characters *)
    | Some ('\n' | '\r' | '\000') ->
        raise (Lex_error (tok_pos st, "invalid string literal"))
    (* normal characters *)
    | Some c ->
        advance st;
        bytes := Char.code c :: !bytes;
        loop ()
    | None -> raise (Lex_error (tok_pos st, "unterminated string literal"))
  in
  loop ()

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
  | "NULL" -> TokInt (0, NoIntSuffix)
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
  | "sizeof" -> TokSizeofKw
  | "typedef" -> TokTypedefKw
  | "struct" -> TokStructKw
  | "float" -> TokFloatKw
  | "double" -> TokDoubleKw
  | "extern" -> TokExternKw
  | s -> TokIdent s
