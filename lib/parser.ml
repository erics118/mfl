open Token

type state = {
  mutable cur_tok : token;
  lex : Lexer.state;
}

exception Parse_error of string

let create lex_st = { cur_tok = Eof; lex = lex_st }
let get_next_token st = st.cur_tok <- Lexer.gettok st.lex

let consume st tok =
  if st.cur_tok = tok then get_next_token st
  else
    raise (Parse_error (Printf.sprintf "expected '%s'" (string_of_token tok)))

let consume_identifier st =
  match st.cur_tok with
  | Identifier name ->
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
  | BinaryOp s -> Ast.precedence (op_of_str s)
  | _ -> -1

(* Parse a comma-separated list of items terminated by ')' *)
let parse_rparen_list st parse_item =
  match st.cur_tok with
  | Rparen -> []
  | _ ->
      let rec loop rev_items =
        let item = parse_item st in
        match st.cur_tok with
        | Comma ->
            consume st Comma;
            loop (item :: rev_items)
        | Rparen -> List.rev (item :: rev_items)
        | _ -> raise (Parse_error "expected ',' or ')'")
      in
      loop []

(* expressions *)
let rec parse_paren_expr st =
  get_next_token st;
  let v = parse_expr st in
  consume st Rparen;
  v

and parse_identifier_expr st =
  let name = consume_identifier st in
  match st.cur_tok with
  | Lparen ->
      consume st Lparen;
      let args = parse_rparen_list st parse_expr in
      consume st Rparen;
      Ast.FuncCall { name; args }
  | _ -> Ast.VarRef name

and parse_primary st =
  match st.cur_tok with
  | Integer n ->
      get_next_token st;
      Ast.IntLiteral n
  | Bool b ->
      get_next_token st;
      Ast.BoolLiteral b
  | Identifier _ -> parse_identifier_expr st
  | Lparen -> parse_paren_expr st
  | BinaryOp "-" ->
      get_next_token st;
      Ast.UnaryOp (Ast.Neg, parse_primary st)
  | UnaryOp "!" ->
      get_next_token st;
      Ast.UnaryOp (Ast.Not, parse_primary st)
  | Eof -> raise (Parse_error "unexpected end of input")
  | _ ->
      raise
        (Parse_error ("unknown token: '" ^ string_of_token st.cur_tok ^ "'"))

and parse_binop_rhs st expr_prec lhs =
  let prec = get_tok_precedence st in
  if prec < expr_prec then lhs
  else
    match st.cur_tok with
    | BinaryOp s ->
        let op = op_of_str s in
        get_next_token st;
        let rhs = parse_primary st in
        let next_prec = get_tok_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec (Ast.BinaryOp (op, lhs, rhs))
    | _ -> lhs [@coverage off]

and parse_binary_expr st =
  let lhs = parse_primary st in
  parse_binop_rhs st 0 lhs

and parse_conditional_expr st =
  let cond = parse_binary_expr st in
  match st.cur_tok with
  | QuestionMark ->
      consume st QuestionMark;
      let then_expr = parse_expr st in
      consume st Colon;
      let else_expr = parse_conditional_expr st in
      Ast.Ternary (cond, then_expr, else_expr)
  | _ -> cond

and parse_expr st = parse_conditional_expr st

let parse_type_name st =
  match st.cur_tok with
  | IntKw ->
      get_next_token st;
      Ast.VarType "int"
  | BoolKw ->
      get_next_token st;
      Ast.VarType "bool"
  | Identifier type_name ->
      get_next_token st;
      Ast.VarType type_name
  | _ -> raise (Parse_error "expected type") [@coverage off]

let parse_return_stmt st =
  consume st ReturnKw;
  match st.cur_tok with
  | Semicolon ->
      consume st Semicolon;
      Ast.ReturnStmt None
  | _ ->
      let e = parse_expr st in
      consume st Semicolon;
      Ast.ReturnStmt (Some e)

let looks_like_definition st =
  match st.cur_tok with
  | IntKw | BoolKw -> true
  | Identifier _ -> (
      match Lexer.peek_next_token st.lex with
      | Identifier _ -> true
      | _ -> false)
  | _ -> false

(* statements *)
let rec parse_statement st =
  match st.cur_tok with
  | LBrace ->
      consume st LBrace;
      Ast.CompoundStmt (parse_compound_stmt st [])
  | ReturnKw -> parse_return_stmt st
  | _ when looks_like_definition st -> parse_def st
  | Semicolon ->
      consume st Semicolon;
      Ast.EmptyStmt
  | _ ->
      let e = parse_expr st in
      consume st Semicolon;
      Ast.Statement e

and parse_compound_stmt st rev_stmts =
  match st.cur_tok with
  | RBrace ->
      consume st RBrace;
      List.rev rev_stmts
  | Eof -> raise (Parse_error "expected '}'")
  | _ ->
      let stmt = parse_statement st in
      parse_compound_stmt st (stmt :: rev_stmts)

and parse_param st =
  let param_type = parse_type_name st in
  let name = consume_identifier st in
  (param_type, name)

and parse_var_def_tail st var_type name =
  consume st Assign;
  let init = parse_expr st in
  consume st Semicolon;
  Ast.VarDef { var_type; name; init }

and parse_func_def_tail st ret_type name =
  consume st Lparen;
  let params = parse_rparen_list st parse_param in
  consume st Rparen;
  consume st LBrace;
  let body = parse_compound_stmt st [] in
  Ast.FuncDef { ret_type; name; params; body }

and parse_def st =
  let var_type = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  | Assign -> parse_var_def_tail st var_type name
  | Lparen -> parse_func_def_tail st var_type name
  | _ -> raise (Parse_error "expected '='")

let parse input =
  let st = create (Lexer.create input) in
  get_next_token st;
  if st.cur_tok = Eof then raise (Parse_error "unexpected end of input")
  else
    let rec parse_statements rev_stmts =
      let stmt = parse_statement st in
      let rev_stmts = stmt :: rev_stmts in
      match st.cur_tok with
      | Eof -> Ast.CompoundStmt (List.rev rev_stmts)
      | _ -> parse_statements rev_stmts
    in
    parse_statements []
