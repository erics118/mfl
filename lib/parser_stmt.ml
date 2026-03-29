open Token
open Ast
open Parser_state
open Parser_expr
open Parser_types

let looks_like_definition st =
  match st.cur_tok with
  | TokTypedefKw -> true
  | TokIdent _ -> (
      match Lexer.peek_token st.lex with
      | TokIdent _ | TokStar -> true
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
  | TokTypedefKw -> parse_typedef st
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

and parse_return_stmt st =
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

and parse_param st =
  let param_type = parse_type_name st in
  let name = consume_identifier st in
  (* allow for int arr[] in function params, syntactic sugar for int* arr *)
  let param_type =
    match st.cur_tok with
    | TokLBracket ->
        advance st;
        consume st TokRBracket;
        VPtr param_type
    | _ -> param_type
  in
  (param_type, name)

and parse_typedef st =
  let pos = cur_pos st in
  consume st TokTypedefKw;
  let existing_type = parse_type_name st in
  let alias = consume_identifier st in
  consume st TokSemicolon;
  Typedef { pos; existing_type; alias }

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

and parse_array_def_tail st pos var_type name =
  consume st TokLBracket;
  let sz =
    match st.cur_tok with
    | TokInt n ->
        advance st;
        n
    | _ ->
        raise
          (Parse_error (cur_pos st, "array size must be a constant integer"))
  in
  consume st TokRBracket;
  consume st TokSemicolon;
  VarDef { pos; var_type = VArray (var_type, sz); name; init = None }

and parse_declaration st =
  let pos = cur_pos st in
  let var_ty = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  | TokAssign ->
      (* if see =, then expect a init value *)
      parse_var_def_tail st pos var_ty name
  | TokLParen ->
      (* if see (, then expect a function definition *)
      parse_func_def_tail st pos var_ty name
  | TokLBracket ->
      (* if see [, then expect array definition *)
      parse_array_def_tail st pos var_ty name
  | TokSemicolon ->
      (* if see ;, then end the declaration *)
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
