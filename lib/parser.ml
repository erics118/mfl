open Token

type state = {
  mutable cur_tok : token;
  lex : Lexer.state;
}

exception Parse_error of Ast.pos * string

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

let op_of_str s =
  match Ast.op_of_string_opt s with
  | Some op -> op
  | None ->
      raise
        (Parse_error
           (Ast.dummy_pos, Printf.sprintf "unknown operator '%s'" s))
        [@coverage off]

let cur_precedence st =
  match st.cur_tok with
  | TokBinaryOp s -> Ast.precedence (op_of_str s)
  | _ -> -1

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

(* expressions *)
let rec parse_paren_expr st =
  advance st;
  let v = parse_expr st in
  consume st TokRParen;
  v

and parse_identifier_expr st pos =
  let name = consume_identifier st in
  match st.cur_tok with
  | TokLParen ->
      consume st TokLParen;
      let args = parse_rparen_list st parse_expr in
      consume st TokRParen;
      Ast.FuncCall (Ast.Parsed pos, name, args)
  | TokAssign ->
      consume st TokAssign;
      let value = parse_expr st in
      Ast.Assign (Ast.Parsed pos, name, value)
  | _ -> Ast.VarRef (Ast.Parsed pos, name)

and parse_primary st =
  let pos = cur_pos st in
  match st.cur_tok with
  | TokInt n ->
      advance st;
      Ast.IntLiteral (Ast.Parsed pos, n)
  | TokBool b ->
      advance st;
      Ast.BoolLiteral (Ast.Parsed pos, b)
  | TokIdent _ -> parse_identifier_expr st pos
  | TokLParen -> parse_paren_expr st
  | TokBinaryOp "-" ->
      advance st;
      Ast.UnaryOp (Ast.Parsed pos, Ast.Neg, parse_primary st)
  | TokUnaryOp "!" ->
      advance st;
      Ast.UnaryOp (Ast.Parsed pos, Ast.Not, parse_primary st)
  | TokUnaryOp "~" ->
      advance st;
      Ast.UnaryOp (Ast.Parsed pos, Ast.Compl, parse_primary st)
  | TokEof -> raise (Parse_error (pos, "unexpected end of input"))
  | _ ->
      raise
        (Parse_error
           (pos, "unknown token: '" ^ string_of_token st.cur_tok ^ "'"))

and parse_binop_rhs st expr_prec lhs =
  let prec = cur_precedence st in
  if prec < expr_prec then lhs
  else
    match st.cur_tok with
    | TokBinaryOp s ->
        let pos = cur_pos st in
        let op = op_of_str s in
        advance st;
        let rhs = parse_primary st in
        let next_prec = cur_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec
          (Ast.BinaryOp (Ast.Parsed pos, op, lhs, rhs))
    | _ -> lhs [@coverage off]

and parse_binary_expr st =
  let lhs = parse_primary st in
  parse_binop_rhs st 0 lhs

and parse_conditional_expr st =
  let cond = parse_binary_expr st in
  match st.cur_tok with
  | TokQuestion ->
      let pos = cur_pos st in
      consume st TokQuestion;
      let then_expr = parse_expr st in
      consume st TokColon;
      let else_expr = parse_conditional_expr st in
      Ast.Ternary (Ast.Parsed pos, cond, then_expr, else_expr)
  | _ -> cond

and parse_expr st = parse_conditional_expr st

let parse_type_name st =
  match st.cur_tok with
  | TokIntKw ->
      advance st;
      Ast.VarType "int"
  | TokBoolKw ->
      advance st;
      Ast.VarType "bool"
  | TokVoidKw ->
      advance st;
      Ast.VarType "void"
  | TokIdent type_name ->
      advance st;
      Ast.VarType type_name
  | _ -> raise (Parse_error (cur_pos st, "expected type")) [@coverage off]

let parse_return_stmt st =
  consume st TokReturnKw;
  match st.cur_tok with
  | TokSemicolon ->
      consume st TokSemicolon;
      Ast.ReturnStmt None
  | _ ->
      let e = parse_expr st in
      consume st TokSemicolon;
      Ast.ReturnStmt (Some e)

let looks_like_definition st =
  match st.cur_tok with
  | TokIntKw | TokBoolKw | TokVoidKw -> true
  | TokIdent _ -> (
      match Lexer.peek_token st.lex with
      | TokIdent _ -> true
      | _ -> false)
  | _ -> false

(* statements *)
let rec parse_statement st =
  match st.cur_tok with
  | TokLBrace ->
      consume st TokLBrace;
      Ast.CompoundStmt (parse_compound_stmt st [])
  | TokReturnKw -> parse_return_stmt st
  | TokIfKw -> parse_if st
  | TokWhileKw -> parse_while st
  | TokForKw -> parse_for st
  | _ when looks_like_definition st -> parse_declaration st
  | TokSemicolon ->
      consume st TokSemicolon;
      Ast.EmptyStmt
  | _ ->
      let e = parse_expr st in
      consume st TokSemicolon;
      Ast.ExprStmt e

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

and parse_var_def_tail st var_type name =
  consume st TokAssign;
  let init = parse_expr st in
  consume st TokSemicolon;
  Ast.VarDef { var_type; name; init }

and parse_func_def_tail st ret_type name =
  consume st TokLParen;
  let params = parse_rparen_list st parse_param in
  consume st TokRParen;
  consume st TokLBrace;
  let body = parse_compound_stmt st [] in
  Ast.FuncDef { ret_type; name; params; body }

and parse_declaration st =
  let var_type = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  | TokAssign -> parse_var_def_tail st var_type name
  | TokLParen -> parse_func_def_tail st var_type name
  | _ -> raise (Parse_error (cur_pos st, "expected '='"))

and parse_if st =
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
  Ast.If { cond; then_body; else_body }

and parse_while st =
  consume st TokWhileKw;
  consume st TokLParen;
  let cond = parse_expr st in
  consume st TokRParen;
  let body = parse_statement st in
  Ast.WhileLoop { cond; body }

and parse_for st =
  consume st TokForKw;
  consume st TokLParen;
  let init = parse_statement st in
  let cond = parse_expr st in
  consume st TokSemicolon;
  let incr = parse_expr st in
  consume st TokRParen;
  let body = parse_statement st in
  Ast.ForLoop { init; cond; incr; body }

let parse input =
  let st = create (Lexer.create input) in
  advance st;
  if st.cur_tok = TokEof then
    raise (Parse_error (cur_pos st, "unexpected end of input"))
  else
    let rec parse_statements rev_stmts =
      let stmt = parse_statement st in
      let rev_stmts = stmt :: rev_stmts in
      match st.cur_tok with
      | TokEof -> Ast.CompoundStmt (List.rev rev_stmts)
      | _ -> parse_statements rev_stmts
    in
    parse_statements []
