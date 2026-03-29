open Token
open Ast
open Parser_state

(* parses the "long"/"long int" after consuming the first "long" *)
let parse_long_suffix signedness st =
  if st.cur_tok = TokLongKw then begin
    advance st;
    (* ignore the "int" if there is one *)
    if st.cur_tok = TokIntKw then advance st;
    match signedness with
    | `None | `Signed -> VLongLong
    | `Unsigned -> VULongLong
  end
  else begin
    (* ignore the "int" if there is one *)
    if st.cur_tok = TokIntKw then advance st;
    match signedness with
    | `None | `Signed -> VLong
    | `Unsigned -> VULong
  end

(* parses the rest of the integer type, after the "signed"/"unsigned" has been
   consumed. defaults to (unsigned) "int". *)
let parse_int_base signedness st =
  match st.cur_tok with
  | TokCharKw -> begin
      advance st;
      match signedness with
      | `None -> VChar
      | `Signed -> VSChar
      | `Unsigned -> VUChar
    end
  | TokShortKw -> begin
      advance st;
      if st.cur_tok = TokIntKw then advance st;
      match signedness with
      | `None | `Signed -> VShort
      | `Unsigned -> VUShort
    end
  | TokIntKw -> begin
      advance st;
      match signedness with
      | `None | `Signed -> VInt
      | `Unsigned -> VUInt
    end
  | TokLongKw -> begin
      advance st;
      parse_long_suffix signedness st
    end
  | _ -> begin
      match signedness with
      | `None | `Signed -> VInt
      | `Unsigned -> VUInt
    end

(* parse the type of a variable *)
let parse_type_name st =
  let base =
    match st.cur_tok with
    | TokBoolKw ->
        advance st;
        VBool
    | TokVoidKw ->
        advance st;
        VVoid
    | TokCharKw ->
        advance st;
        VChar
    | TokShortKw ->
        advance st;
        if st.cur_tok = TokIntKw then advance st;
        VShort
    | TokIntKw ->
        advance st;
        VInt
    | TokLongKw ->
        advance st;
        parse_long_suffix `None st
    | TokUnsignedKw ->
        advance st;
        parse_int_base `Unsigned st
    | TokSignedKw ->
        advance st;
        parse_int_base `Signed st
    | TokIdent type_name ->
        advance st;
        VNamed type_name
    | _ -> raise (Parse_error (cur_pos st, "expected type")) [@coverage off]
  in
  let rec parse_ptr_suffix ty =
    match st.cur_tok with
    | TokStar ->
        advance st;
        parse_ptr_suffix (VPtr ty)
    | _ -> ty
  in
  parse_ptr_suffix base

(* exhaustive match on token ensures new type keywords are not silently
   missed *)
let is_type_keyword = function
  | TokBoolKw
  | TokVoidKw
  | TokCharKw
  | TokShortKw
  | TokIntKw
  | TokLongKw
  | TokUnsignedKw
  | TokSignedKw -> true
  | TokInt _
  | TokBool _
  | TokChar _
  | TokIdent _
  | TokReturnKw
  | TokIfKw
  | TokElseKw
  | TokWhileKw
  | TokForKw
  | TokBreakKw
  | TokContinueKw
  | TokDoKw
  | TokSizeofKw
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokPercent
  | TokEqEq
  | TokBangEq
  | TokLt
  | TokLtEq
  | TokGt
  | TokGtEq
  | TokAmpAmp
  | TokPipePipe
  | TokAmp
  | TokPipe
  | TokCaret
  | TokLtLt
  | TokGtGt
  | TokBang
  | TokTilde
  | TokPlusPlus
  | TokMinusMinus
  | TokAssign
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokSemicolon
  | TokComma
  | TokQuestion
  | TokColon
  | TokEof -> false

let is_type_token st = is_type_keyword st.cur_tok
