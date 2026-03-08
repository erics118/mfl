type state = {
  mutable cur_tok : Lexer.token;
  lex : Lexer.state;
}

exception Parse_error of string

let create lex_st = { cur_tok = Lexer.Eof; lex = lex_st }
let get_next_token st = st.cur_tok <- Lexer.gettok st.lex

let op_of_char = function
  | '+' -> Ast.Add
  | '-' -> Ast.Sub
  | '*' -> Ast.Mul
  | '/' -> Ast.Div
  | c -> raise (Parse_error (Printf.sprintf "unknown operator '%c'" c))

let char_of_op = function
  | Ast.Add -> '+'
  | Ast.Sub -> '-'
  | Ast.Mul -> '*'
  | Ast.Div -> '/'

let get_tok_precedence st =
  match st.cur_tok with
  | Lexer.BinaryOp c -> Ast.precedence (op_of_char c)
  | _ -> -1

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
  | Lexer.Number n ->
      get_next_token st;
      Ast.Number n
  | Lexer.Lparen -> parse_paren_expr st
  | Eof -> raise (Parse_error "unexpected end of input")
  | _ ->
      raise (Parse_error ("unknown token: " ^ Lexer.string_of_token st.cur_tok))

and parse_binop_rhs st expr_prec lhs =
  let prec = get_tok_precedence st in
  if prec < expr_prec then lhs
  else
    match st.cur_tok with
    | Lexer.BinaryOp c ->
        let op = op_of_char c in
        get_next_token st;
        let rhs = parse_primary st in
        let next_prec = get_tok_precedence st in
        let rhs =
          if prec < next_prec then parse_binop_rhs st (prec + 1) rhs else rhs
        in
        parse_binop_rhs st expr_prec (Ast.Binary (op, lhs, rhs))
    | _ -> lhs

and parse_expr st =
  let lhs = parse_primary st in
  parse_binop_rhs st 0 lhs

let parse input =
  let st = create (Lexer.create input) in
  get_next_token st;
  let expr = parse_expr st in
  match st.cur_tok with
  | Lexer.Eof -> expr
  | _ -> raise (Parse_error "unexpected trailing input")
