type state = {
  mutable cur_tok : Lexer.token;
  lex : Lexer.state;
}

exception Parse_error of string

let create lex_st = { cur_tok = Lexer.Eof; lex = lex_st }
let get_next_token st = st.cur_tok <- Lexer.gettok st.lex

let consume st tok =
  if st.cur_tok = tok then get_next_token st
  else
    raise
      (Parse_error (Printf.sprintf "expected '%s'" (Lexer.string_of_token tok)))

let consume_identifier st =
  match st.cur_tok with
  | Lexer.Identifier name ->
      get_next_token st;
      name
  | _ -> raise (Parse_error "expected identifier")

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
  consume st Lexer.Rparen;
  v

and parse_primary st =
  match st.cur_tok with
  | Lexer.Integer n ->
      get_next_token st;
      Ast.IntLiteral n
  | Lexer.Bool b ->
      get_next_token st;
      Ast.BoolLiteral b
  | Lexer.Identifier name ->
      get_next_token st;
      Ast.VarRef name
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

let parse_return_stmt st =
  consume st Lexer.ReturnKw;
  match st.cur_tok with
  | Lexer.Semicolon ->
      consume st Lexer.Semicolon;
      Ast.ReturnStmt None
  | _ ->
      let e = parse_expr st in
      consume st Lexer.Semicolon;
      Ast.ReturnStmt (Some e)

let looks_like_definition st =
  match st.cur_tok with
  | Lexer.IntKw | Lexer.BoolKw -> true
  | Lexer.Identifier _ -> (
      let lex_lookahead : Lexer.state =
        { input = st.lex.input; pos = st.lex.pos }
      in
      match Lexer.gettok lex_lookahead with
      | Lexer.Identifier _ -> true
      | _ -> false)
  | _ -> false

(* statements *)
let rec parse_statement st =
  match st.cur_tok with
  | Lexer.LBrace ->
      consume st LBrace;
      parse_compound_stmt st []
  | Lexer.ReturnKw -> parse_return_stmt st
  | _ when looks_like_definition st -> parse_def st
  | Lexer.Semicolon ->
      consume st Lexer.Semicolon;
      Ast.EmptyStmt
  | _ ->
      let e = parse_expr st in
      consume st Lexer.Semicolon;
      Ast.Statement e

and parse_compound_stmt st rev_stmts =
  match st.cur_tok with
  | Lexer.RBrace ->
      consume st Lexer.RBrace;
      Ast.CompoundStmt (List.rev rev_stmts)
  | Lexer.Eof -> raise (Parse_error "expected '}'")
  | _ ->
      let stmt = parse_statement st in
      parse_compound_stmt st (stmt :: rev_stmts)

and parse_param st =
  let param_type = parse_type_name st in
  let name = consume_identifier st in
  (param_type, name)

and parse_param_list st =
  match st.cur_tok with
  | Lexer.Rparen -> []
  | _ ->
      let rec loop rev_params =
        let p = parse_param st in
        match st.cur_tok with
        | Lexer.Comma ->
            consume st Lexer.Comma;
            loop (p :: rev_params)
        | Lexer.Rparen -> List.rev (p :: rev_params)
        | _ -> raise (Parse_error "expected ',' or ')'")
      in
      loop []

and parse_var_def_tail st var_type name =
  consume st Lexer.Assign;
  let init = parse_expr st in
  consume st Lexer.Semicolon;
  Ast.VarDef { var_type; name; init }

and parse_func_def_tail st ret_type name =
  consume st Lexer.Lparen;
  let params = parse_param_list st in
  consume st Lexer.Rparen;
  consume st Lexer.LBrace;
  let body = parse_compound_stmt st [] in
  Ast.FuncDef { ret_type; name; params; body }

and parse_def st =
  let var_type = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  | Lexer.Assign -> parse_var_def_tail st var_type name
  | Lexer.Lparen -> parse_func_def_tail st var_type name
  | _ -> raise (Parse_error "expected '='")

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
