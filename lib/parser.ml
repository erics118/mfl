type state = {
  mutable cur_tok : Lexer.token;
  lex : Lexer.state;
}

exception Parse_error of string

let create lex_st = { cur_tok = Lexer.Eof; lex = lex_st }
let get_next_token st = st.cur_tok <- Lexer.gettok st.lex

let op_of_str s =
  match Ast.op_of_string_opt s with
  | Some op -> op
  | None ->
      raise
        (Parse_error (Printf.sprintf "unknown operator '%s'" s)) [@coverage off]

let get_tok_precedence st =
  match st.cur_tok with
  | Lexer.BinaryOp s -> Ast.precedence (op_of_str s)
  | _ -> -1

(* expressions *)
let rec parse_paren_expr st =
  get_next_token st;
  let v = parse_expr st in
  match st.cur_tok with
  | Lexer.Rparen ->
      get_next_token st;
      v
  | _ -> raise (Parse_error "expected ')'")

and parse_primary st =
  match st.cur_tok with
  | Lexer.Integer n ->
      get_next_token st;
      Ast.IntLiteral n
  | Lexer.Bool b ->
      get_next_token st;
      Ast.BoolLiteral b
  | Lexer.Lparen -> parse_paren_expr st
  | Lexer.BinaryOp "-" ->
      get_next_token st;
      Ast.UnaryOp (Ast.Neg, parse_primary st)
  | Lexer.UnaryOp "!" ->
      get_next_token st;
      Ast.UnaryOp (Ast.Not, parse_primary st)
  | Eof -> raise (Parse_error "unexpected end of input")
  | _ ->
      raise
        (Parse_error
           ("unknown token: '" ^ Lexer.string_of_token st.cur_tok ^ "'"))

and parse_binop_rhs st expr_prec lhs =
  let prec = get_tok_precedence st in
  if prec < expr_prec then lhs
  else
    match st.cur_tok with
    | Lexer.BinaryOp s ->
        let op = op_of_str s in
        get_next_token st;
        let rhs = parse_primary st in
        let next_prec = get_tok_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec (Ast.BinaryOp (op, lhs, rhs))
    | _ -> lhs [@coverage off]

and parse_expr st =
  let lhs = parse_primary st in
  parse_binop_rhs st 0 lhs

let parse_type_name st =
  match st.cur_tok with
  | Lexer.IntKw ->
      get_next_token st;
      Ast.VarType "int"
  | Lexer.BoolKw ->
      get_next_token st;
      Ast.VarType "bool"
  | Lexer.Identifier type_name ->
      get_next_token st;
      Ast.VarType type_name
  | _ -> raise (Parse_error "expected type") [@coverage off]

let parse_var_def st =
  let var_type = parse_type_name st in
  let name =
    match st.cur_tok with
    | Lexer.Identifier name ->
        get_next_token st;
        name
    | _ -> raise (Parse_error "expected identifier")
  in
  begin match st.cur_tok with
  | Lexer.Assign -> get_next_token st
  | _ -> raise (Parse_error "expected '='")
  end;
  let init = parse_expr st in
  match st.cur_tok with
  | Lexer.Semicolon ->
      get_next_token st;
      Ast.VarDef (var_type, name, init)
  | _ -> raise (Parse_error "expected ';'")

(* statements *)
let rec parse_statement st =
  match st.cur_tok with
  | Lexer.LBrace ->
      get_next_token st;
      parse_compound_stmt st []
  | Lexer.IntKw | Lexer.BoolKw | Lexer.Identifier _ -> parse_var_def st
  | Lexer.Semicolon ->
      get_next_token st;
      Ast.EmptyStmt
  | _ -> (
      let e = parse_expr st in
      match st.cur_tok with
      | Lexer.Semicolon ->
          get_next_token st;
          Ast.Statement e
      | _ -> raise (Parse_error "expected ';'"))

and parse_compound_stmt st rev_stmts =
  match st.cur_tok with
  | Lexer.RBrace ->
      get_next_token st;
      Ast.CompoundStmt (List.rev rev_stmts)
  | Lexer.Eof -> raise (Parse_error "expected '}'")
  | _ ->
      let stmt = parse_statement st in
      parse_compound_stmt st (stmt :: rev_stmts)

let parse input =
  let st = create (Lexer.create input) in
  get_next_token st;
  if st.cur_tok = Lexer.Eof then raise (Parse_error "unexpected end of input")
  else
    let rec parse_statements rev_stmts =
      let stmt = parse_statement st in
      let rev_stmts = stmt :: rev_stmts in
      match st.cur_tok with
      | Lexer.Eof -> Ast.CompoundStmt (List.rev rev_stmts)
      | _ -> parse_statements rev_stmts
    in
    parse_statements []
