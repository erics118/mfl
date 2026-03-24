(** parser entrypoint *)

open Token
open Ast

type state = {
  mutable cur_tok : token;
  lex : Lexer.state;
}

(** raised on parse errors *)
exception Parse_error of pos * string

let create lex_st = { cur_tok = TokEof; lex = lex_st }
let advance st = st.cur_tok <- Lexer.next_token st.lex

(* position of the current token *)
let cur_pos st = Lexer.tok_pos st.lex

let consume st tok =
  if st.cur_tok = tok then advance st
  else
    raise
      (Parse_error
         (cur_pos st, Printf.sprintf "expected '%s'" (string_of_token tok)))

let consume_identifier st =
  match st.cur_tok with
  | TokIdent name ->
      advance st;
      name
  | _ -> raise (Parse_error (cur_pos st, "expected identifier"))

let op_of_tok = function
  | TokPlus -> Some Add
  | TokMinus -> Some Sub
  | TokStar -> Some Mul
  | TokSlash -> Some Div
  | TokPercent -> Some Mod
  | TokEqEq -> Some Equal
  | TokBangEq -> Some Neq
  | TokLt -> Some Less
  | TokLtEq -> Some Leq
  | TokGt -> Some Greater
  | TokGtEq -> Some Geq
  | TokAmpAmp -> Some And
  | TokPipePipe -> Some Or
  | TokAmp -> Some BitAnd
  | TokPipe -> Some BitOr
  | TokCaret -> Some BitXor
  | TokLtLt -> Some LShift
  | TokGtGt -> Some RShift
  | _ -> None

let cur_precedence st =
  match op_of_tok st.cur_tok with
  | Some op -> precedence op
  | None -> -1

(* Parse a comma-separated list of items terminated by ')' *)
let parse_rparen_list st parse_item =
  match st.cur_tok with
  | TokRParen -> []
  | _ ->
      let rec loop rev_items =
        let item = parse_item st in
        match st.cur_tok with
        | TokComma ->
            consume st TokComma;
            loop (item :: rev_items)
        | TokRParen -> List.rev (item :: rev_items)
        | _ -> raise (Parse_error (cur_pos st, "expected ',' or ')'"))
      in
      loop []

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
  | TokSemicolon
  | TokComma
  | TokQuestion
  | TokColon
  | TokEof -> false

let is_type_token st = is_type_keyword st.cur_tok

(* expressions *)
let rec parse_paren_expr st =
  let pos = cur_pos st in
  consume st TokLParen;
  if is_type_token st then begin
    (* type cast, (type)expr *)
    let ty = parse_type_name st in
    consume st TokRParen;
    let e = parse_postfix st in
    Cast (Parsed pos, ty, e)
  end
  else begin
    (* normal paren expr *)
    let e = parse_expr st in
    consume st TokRParen;
    e
  end

and parse_identifier_expr st pos =
  let name = consume_identifier st in
  match st.cur_tok with
  | TokLParen ->
      consume st TokLParen;
      let args = parse_rparen_list st parse_expr in
      consume st TokRParen;
      FuncCall (Parsed pos, name, args)
  | _ -> VarRef (Parsed pos, name)

and parse_primary st =
  let pos = cur_pos st in
  match st.cur_tok with
  | TokInt n ->
      advance st;
      IntLiteral (Parsed pos, n)
  | TokBool b ->
      advance st;
      BoolLiteral (Parsed pos, b)
  | TokChar c ->
      advance st;
      CharLiteral (Parsed pos, c)
  | TokIdent _ -> parse_identifier_expr st pos
  | TokLParen -> parse_paren_expr st
  | TokMinus ->
      advance st;
      UnaryOp (Parsed pos, Neg, parse_postfix st)
  | TokAmp ->
      advance st;
      UnaryOp (Parsed pos, AddrOf, parse_postfix st)
  | TokStar ->
      advance st;
      UnaryOp (Parsed pos, Deref, parse_postfix st)
  | TokBang ->
      advance st;
      UnaryOp (Parsed pos, Not, parse_postfix st)
  | TokTilde ->
      advance st;
      UnaryOp (Parsed pos, Compl, parse_postfix st)
  | TokEof -> raise (Parse_error (pos, "unexpected end of input"))
  | TokPlusPlus ->
      advance st;
      let e = parse_postfix st in
      PreInc (Parsed pos, e)
  | TokMinusMinus ->
      advance st;
      let e = parse_postfix st in
      PreDec (Parsed pos, e)
  | _ ->
      raise
        (Parse_error (pos, "unknown token: '" ^ string_of_token st.cur_tok ^ "'"))

(* parse postfix operators, chaining as many as appear *)
and parse_postfix st =
  let e = parse_primary st in
  let rec loop e =
    let pos = cur_pos st in
    match st.cur_tok with
    | TokPlusPlus ->
        advance st;
        loop (PostInc (Parsed pos, e))
    | TokMinusMinus ->
        advance st;
        loop (PostDec (Parsed pos, e))
    | _ -> e
  in
  loop e

and parse_binop_rhs st expr_prec lhs =
  let prec = cur_precedence st in
  if prec < expr_prec then lhs
  else
    match op_of_tok st.cur_tok with
    | Some op ->
        let pos = cur_pos st in
        advance st;
        (* parse a postfix expr if exists *)
        let rhs = parse_postfix st in
        let next_prec = cur_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec (BinaryOp (Parsed pos, op, lhs, rhs))
    | None -> lhs [@coverage off]

and parse_binary_expr st =
  (* parse a postfix expr if exists *)
  let lhs = parse_postfix st in
  parse_binop_rhs st 0 lhs

and parse_conditional_expr st =
  let cond = parse_binary_expr st in
  match st.cur_tok with
  | TokQuestion ->
      let pos = cur_pos st in
      consume st TokQuestion;
      let then_e = parse_expr st in
      consume st TokColon;
      let else_e = parse_conditional_expr st in
      Ternary (Parsed pos, cond, then_e, else_e)
  | _ -> cond

and parse_expr (st : state) : parsed expr =
  let pos = cur_pos st in
  let e = parse_conditional_expr st in
  (* after parsing the full expression, check for a assign token *)
  match st.cur_tok with
  | TokAssign ->
      consume st TokAssign;
      let value = parse_expr st in
      Assign (Parsed pos, e, value)
  | _ -> e

let parse_return_stmt st =
  let pos = cur_pos st in
  consume st TokReturnKw;
  match st.cur_tok with
  | TokSemicolon ->
      consume st TokSemicolon;
      ReturnStmt (pos, None)
  | _ ->
      let e = parse_expr st in
      consume st TokSemicolon;
      ReturnStmt (pos, Some e)

let looks_like_definition st =
  match st.cur_tok with
  | TokIdent _ -> (
      match Lexer.peek_token st.lex with
      | TokIdent _ -> true
      | _ -> false)
  | tok -> is_type_keyword tok

(* statements *)
let rec parse_statement st =
  let pos = cur_pos st in
  match st.cur_tok with
  | TokLBrace ->
      consume st TokLBrace;
      CompoundStmt (pos, parse_compound_stmt st [])
  | TokReturnKw -> parse_return_stmt st
  | TokBreakKw ->
      advance st;
      consume st TokSemicolon;
      BreakStmt pos
  | TokContinueKw ->
      advance st;
      consume st TokSemicolon;
      ContinueStmt pos
  | TokIfKw -> parse_if st
  | TokWhileKw -> parse_while st
  | TokForKw -> parse_for st
  | TokDoKw -> parse_do_while st
  | _ when looks_like_definition st -> parse_declaration st
  | TokSemicolon ->
      consume st TokSemicolon;
      EmptyStmt pos
  | _ ->
      let e = parse_expr st in
      consume st TokSemicolon;
      ExprStmt (pos, e)

and parse_compound_stmt st rev_stmts =
  match st.cur_tok with
  | TokRBrace ->
      consume st TokRBrace;
      List.rev rev_stmts
  | TokEof -> raise (Parse_error (cur_pos st, "expected '}'"))
  | _ ->
      let stmt = parse_statement st in
      parse_compound_stmt st (stmt :: rev_stmts)

and parse_param st =
  let param_type = parse_type_name st in
  let name = consume_identifier st in
  (param_type, name)

and parse_var_def_tail st pos var_type name =
  consume st TokAssign;
  let init = parse_expr st in
  consume st TokSemicolon;
  VarDef { pos; var_type; name; init = Some init }

and parse_func_def_tail st pos ret_type name =
  consume st TokLParen;
  let params = parse_rparen_list st parse_param in
  consume st TokRParen;
  consume st TokLBrace;
  let body = parse_compound_stmt st [] in
  FuncDef { pos; ret_type; name; params; body }

and parse_declaration st =
  let pos = cur_pos st in
  let var_ty = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  (* if see =, then expect a init value *)
  | TokAssign -> parse_var_def_tail st pos var_ty name
  (* if see (, then expect a function definition *)
  | TokLParen -> parse_func_def_tail st pos var_ty name
  (* if see ;, then end the declaration *)
  | TokSemicolon ->
      (* if *)
      consume st TokSemicolon;
      VarDef { pos; var_type = var_ty; name; init = None }
  | _ -> raise (Parse_error (cur_pos st, "expected '='"))

and parse_if st =
  let pos = cur_pos st in
  consume st TokIfKw;
  consume st TokLParen;
  let cond = parse_expr st in
  consume st TokRParen;
  let then_body = parse_statement st in
  let else_body =
    match st.cur_tok with
    | TokElseKw ->
        consume st TokElseKw;
        let s = parse_statement st in
        Some s
    | _ -> None
  in
  If { pos; cond; then_body; else_body }

and parse_while st =
  let pos = cur_pos st in
  consume st TokWhileKw;
  consume st TokLParen;
  let cond = parse_expr st in
  consume st TokRParen;
  let body = parse_statement st in
  WhileLoop { pos; cond; body }

and parse_do_while st =
  let pos = cur_pos st in
  consume st TokDoKw;
  let body = parse_statement st in
  consume st TokWhileKw;
  consume st TokLParen;
  let cond = parse_expr st in
  consume st TokRParen;
  consume st TokSemicolon;
  DoWhileLoop { pos; body; cond }

and parse_for st =
  let pos = cur_pos st in
  consume st TokForKw;
  consume st TokLParen;
  (* can be an empty stmt *)
  let init = parse_statement st in
  (* if ;, then empty *)
  let cond = if st.cur_tok = TokSemicolon then None else Some (parse_expr st) in
  consume st TokSemicolon;
  let incr = if st.cur_tok = TokRParen then None else Some (parse_expr st) in
  consume st TokRParen;
  let body = parse_statement st in
  ForLoop { pos; init; cond; incr; body }

(** [parse input] parses [input] into an ast
    @raise Parse_error if the input is malformed *)
let parse (input : string) : parsed stmt =
  let st = create (Lexer.create input) in
  advance st;
  if st.cur_tok = TokEof then
    raise (Parse_error (cur_pos st, "unexpected end of input"))
  else
    let program_pos = cur_pos st in
    let rec parse_statements rev_stmts =
      let stmt = parse_statement st in
      let rev_stmts = stmt :: rev_stmts in
      match st.cur_tok with
      | TokEof -> CompoundStmt (program_pos, List.rev rev_stmts)
      | _ -> parse_statements rev_stmts
    in
    parse_statements []
