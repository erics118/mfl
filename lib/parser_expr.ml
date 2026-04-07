open Token
open Ast
open Parser_state
open Parser_types

(* expressions *)
let rec parse_paren_expr st =
  let pos = cur_pos st in
  consume st TokLParen;
  if is_type_token st then begin
    (* type cast, (type)expr *)
    let ty = parse_type_name st in
    consume st TokRParen;
    let e = parse_unary st in
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
  | TokInt (n, suffix) ->
      advance st;
      IntLiteral (Parsed pos, n, suffix)
  | TokFloat f ->
      advance st;
      FloatLiteral (Parsed pos, f)
  | TokDouble f ->
      advance st;
      DoubleLiteral (Parsed pos, f)
  | TokLongDouble f ->
      advance st;
      LongDoubleLiteral (Parsed pos, f)
  | TokBool b ->
      advance st;
      BoolLiteral (Parsed pos, b)
  | TokChar c ->
      advance st;
      CharLiteral (Parsed pos, c)
  | TokString s ->
      advance st;
      StringLiteral (Parsed pos, s)
  | TokIdent _ -> parse_identifier_expr st pos
  | TokLParen -> parse_paren_expr st
  | TokEof -> raise (Parse_error (pos, "unexpected end of input"))
  | _ ->
      raise
        (Parse_error (pos, "unknown token: '" ^ string_of_token st.cur_tok ^ "'"))

(* parse postfix operators, chaining as many as appear *)
and parse_postfix st =
  let e = parse_primary st in
  let rec loop e =
    let pos = cur_pos st in
    match st.cur_tok with
    | TokLBracket ->
        (* subscript is postfix operator *)
        advance st;
        let i = parse_expr st in
        consume st TokRBracket;
        loop (Subscript (Parsed pos, e, i))
    | TokDot ->
        (* member access, e.field *)
        advance st;
        let field = consume_identifier st in
        loop (MemberAccess (Parsed pos, e, field))
    | TokArrow ->
        (* pointer member access, e->field, desugar to ( *e).field *)
        advance st;
        let field = consume_identifier st in
        let derefed = UnaryOp (Parsed pos, Deref, e) in
        loop (MemberAccess (Parsed pos, derefed, field))
    | TokPlusPlus ->
        advance st;
        loop (PostInc (Parsed pos, e))
    | TokMinusMinus ->
        advance st;
        loop (PostDec (Parsed pos, e))
    | _ -> e
  in
  loop e

and parse_sizeof_expr st =
  let pos = cur_pos st in
  consume st TokSizeofKw;
  match st.cur_tok with
  | TokLParen ->
      consume st TokLParen;
      if is_type_token st then begin
        let ty = parse_type_name st in
        consume st TokRParen;
        SizeofType (Parsed pos, ty)
      end
      else begin
        let e = parse_expr st in
        consume st TokRParen;
        SizeofExpr (Parsed pos, e)
      end
  | _ ->
      let e = parse_unary st in
      SizeofExpr (Parsed pos, e)

and parse_unary st =
  let pos = cur_pos st in
  match st.cur_tok with
  | TokMinus ->
      advance st;
      UnaryOp (Parsed pos, Neg, parse_unary st)
  | TokAmp ->
      advance st;
      UnaryOp (Parsed pos, AddrOf, parse_unary st)
  | TokStar ->
      advance st;
      UnaryOp (Parsed pos, Deref, parse_unary st)
  | TokBang ->
      advance st;
      UnaryOp (Parsed pos, Not, parse_unary st)
  | TokTilde ->
      advance st;
      UnaryOp (Parsed pos, Compl, parse_unary st)
  | TokPlusPlus ->
      advance st;
      let e = parse_unary st in
      PreInc (Parsed pos, e)
  | TokMinusMinus ->
      advance st;
      let e = parse_unary st in
      PreDec (Parsed pos, e)
  | TokSizeofKw -> parse_sizeof_expr st
  | _ -> parse_postfix st

and parse_binop_rhs st expr_prec lhs =
  let prec = cur_precedence st in
  if prec < expr_prec then lhs
  else
    match op_of_tok st.cur_tok with
    | Some op ->
        let pos = cur_pos st in
        advance st;
        (* parse a unary expr if exists *)
        let rhs = parse_unary st in
        let next_prec = cur_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec (BinaryOp (Parsed pos, op, lhs, rhs))
    | None -> lhs [@coverage off]

and parse_binary_expr st =
  (* parse a unary expr if exists *)
  let lhs = parse_unary st in
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

and compound_assign_op_of_tok = function
  | TokPlusAssign -> Some Add
  | TokMinusAssign -> Some Sub
  | TokStarAssign -> Some Mul
  | TokSlashAssign -> Some Div
  | TokPercentAssign -> Some Mod
  | TokAmpAssign -> Some BitAnd
  | TokPipeAssign -> Some BitOr
  | TokCaretAssign -> Some BitXor
  | TokLtLtAssign -> Some LShift
  | TokGtGtAssign -> Some RShift
  | _ -> None

and parse_expr (st : state) : parsed expr =
  let pos = cur_pos st in
  let e = parse_conditional_expr st in
  (* after parsing the full expression, check for assign tokens *)
  match st.cur_tok with
  | TokAssign ->
      consume st TokAssign;
      let value = parse_expr st in
      Assign (Parsed pos, e, value)
  | tok -> begin
      match compound_assign_op_of_tok tok with
      | Some op ->
          advance st;
          let value = parse_expr st in
          CompoundAssign (Parsed pos, op, e, value)
      | None -> e
    end
