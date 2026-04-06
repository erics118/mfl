open Token
open Ast
open Parser_state
open Parser_expr
open Parser_types

let looks_like_definition st =
  match st.cur_tok with
  | TokTypedefKw | TokStructKw | TokExternKw -> true
  | tok when is_type_keyword tok -> true
  | TokIdent _ -> (
      match Lexer.peek_token st.lex with
      | TokIdent _ | TokStar -> true
      | _ -> false)
  | _ -> false

(* statements *)
let rec parse_statement st =
  let pos = cur_pos st in
  match st.cur_tok with
  | TokLBrace ->
      consume st TokLBrace;
      CompoundStmt (pos, parse_scoped_compound_stmt st)
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
  | TokExternKw -> parse_declaration ~is_extern:true st
  | TokStructKw -> parse_struct_or_var st
  | _ when looks_like_definition st -> parse_declaration ~is_extern:false st
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

and parse_scoped_compound_stmt st =
  push_scope st;
  let stmts =
    match parse_compound_stmt st [] with
    | stmts -> stmts
    | exception exn ->
        pop_scope st;
        raise exn
  in
  pop_scope st;
  stmts

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

(** returns a tuple of (params_type * name list, is_variadic) *)
and parse_func_params st =
  match st.cur_tok with
  | TokRParen -> ([], false)
  | _ ->
      let rec loop rev_params =
        match st.cur_tok with
        (* variadic *)
        | TokEllipsis -> begin
            advance st;
            match st.cur_tok with
            | TokRParen -> (List.rev rev_params, true)
            | _ -> raise (Parse_error (cur_pos st, "expected ')'"))
          end
        (* normal *)
        | _ -> begin
            let param = parse_param st in
            match st.cur_tok with
            | TokComma ->
                consume st TokComma;
                loop (param :: rev_params)
            | TokRParen -> (List.rev (param :: rev_params), false)
            | _ -> raise (Parse_error (cur_pos st, "expected ',' or ')'"))
          end
      in
      loop []

and parse_array_suffix st source_type =
  consume st TokLBracket;
  let sz =
    match st.cur_tok with
    | TokInt (n, _) ->
        advance st;
        n
    | _ ->
        raise
          (Parse_error (cur_pos st, "array size must be a constant integer"))
  in
  consume st TokRBracket;
  VArray (source_type, sz)

(* parse the body of a struct: { field; field; ... } returning the field list *)
and parse_struct_body st =
  consume st TokLBrace;
  let rec go acc =
    match st.cur_tok with
    | TokRBrace ->
        consume st TokRBrace;
        List.rev acc
    | TokEof -> raise (Parse_error (cur_pos st, "expected '}'"))
    | _ ->
        let field_type = parse_type_name st in
        let field_name = consume_identifier st in
        let field_type =
          match st.cur_tok with
          | TokLBracket -> parse_array_suffix st field_type
          | _ -> field_type
        in
        consume st TokSemicolon;
        go ((field_type, field_name) :: acc)
  in
  go []

(* handle all struct-prefixed statements: struct Tag { ... }; -> StructDef
   struct Tag { ... } var; -> StructDef with var_name struct Tag var; -> VarDef
   struct Tag var = init; -> VarDef with init struct Tag *var; -> VarDef
   (pointer) struct Tag func(params){...} -> FuncDef struct Tag arr[n]; ->
   VarDef (array) struct Tag; -> forward decl, emitted as EmptyStmt *)
and parse_struct_or_var st =
  let pos = cur_pos st in
  consume st TokStructKw;
  let tag = consume_identifier st in
  match st.cur_tok with
  | TokLBrace -> (
      (* struct Tag { ... } [var_name]; *)
      let fields = parse_struct_body st in
      match st.cur_tok with
      | TokSemicolon ->
          consume st TokSemicolon;
          StructDef { pos; tag; fields; var_name = None }
      | TokIdent _ ->
          let var_name = consume_identifier st in
          consume st TokSemicolon;
          StructDef { pos; tag; fields; var_name = Some var_name }
      | _ -> raise (Parse_error (cur_pos st, "expected ';' or variable name")))
  | TokSemicolon ->
      (* struct Tag; - forward declaration, no-op *)
      consume st TokSemicolon;
      EmptyStmt pos
  | _ -> (
      (* struct Tag [*...] name [= init | (params){body} | [n]]; *)
      let base_type = VStruct tag in
      let base_type =
        let rec loop ty =
          match st.cur_tok with
          | TokStar ->
              advance st;
              loop (VPtr ty)
          | _ -> ty
        in
        loop base_type
      in
      let name = consume_identifier st in
      match st.cur_tok with
      | TokSemicolon ->
          consume st TokSemicolon;
          VarDef { pos; source_type = base_type; name; init = None }
      | TokLBracket ->
          let source_type = parse_array_suffix st base_type in
          consume st TokSemicolon;
          VarDef { pos; source_type; name; init = None }
      | TokAssign -> parse_var_def_tail st pos base_type name
      | TokLParen -> parse_func_tail st pos base_type name ~is_extern:false
      | _ -> raise (Parse_error (cur_pos st, "expected ';', '=', '(', or '['")))

and parse_typedef st =
  let pos = cur_pos st in
  consume st TokTypedefKw;
  match st.cur_tok with
  | TokStructKw ->
      (* typedef struct [Tag] { ... } Alias; or typedef struct Tag Alias; *)
      advance st;
      let tag_opt, body_opt =
        match st.cur_tok with
        | TokLBrace ->
            (* typedef struct { ... } Alias; *)
            (None, Some (parse_struct_body st))
        | TokIdent tag_name ->
            advance st;
            if st.cur_tok = TokLBrace then
              (* typedef struct Tag { ... } Alias; *)
              (Some tag_name, Some (parse_struct_body st))
            else
              (* typedef struct Tag Alias; references existing struct *)
              (Some tag_name, None)
        | _ -> raise (Parse_error (cur_pos st, "expected struct tag or '{'"))
      in
      (* count number of ptr stars before the alias *)
      let num_stars =
        let n = ref 0 in
        while st.cur_tok = TokStar do
          advance st;
          incr n
        done;
        !n
      in
      let alias = consume_identifier st in
      consume st TokSemicolon;
      let tag, struct_def =
        match (tag_opt, body_opt) with
        | None, Some fields ->
            (* anonymous struct: generate a synthetic tag *)
            let anon = "__anon_" ^ alias in
            (anon, Some (anon, fields))
        | Some tag, Some fields -> (tag, Some (tag, fields))
        | Some tag, None -> (tag, None)
        | None, None -> assert false
      in
      let existing_type =
        let rec wrap n t = if n = 0 then t else wrap (n - 1) (VPtr t) in
        wrap num_stars (VStruct tag)
      in
      define_typedef st alias;
      Typedef { pos; struct_def; existing_type; alias }
  | _ ->
      let existing_type = parse_type_name st in
      let alias = consume_identifier st in
      let existing_type =
        match st.cur_tok with
        | TokLBracket -> parse_array_suffix st existing_type
        | _ -> existing_type
      in
      consume st TokSemicolon;
      define_typedef st alias;
      Typedef { pos; struct_def = None; existing_type; alias }

and parse_var_def_tail st pos source_type name =
  consume st TokAssign;
  let init = parse_expr st in
  consume st TokSemicolon;
  VarDef { pos; source_type; name; init = Some init }

and parse_func_tail st pos ret_type name ~is_extern =
  consume st TokLParen;
  let params, is_variadic = parse_func_params st in
  consume st TokRParen;
  begin match st.cur_tok with
  (* function declaration *)
  | TokSemicolon ->
      consume st TokSemicolon;
      FuncDecl { pos; ret_type; name; params; is_extern; is_variadic }
  (* function definition *)
  | TokLBrace ->
      if is_extern then
        raise (Parse_error (cur_pos st, "extern function cannot have a body"));
      consume st TokLBrace;
      let body = parse_scoped_compound_stmt st in
      FuncDef { pos; ret_type; name; params; is_variadic; body }
  (* neither, so error *)
  | _ -> raise (Parse_error (cur_pos st, "expected ';' or '{'"))
  end

and parse_array_def_tail st pos source_type name =
  let source_type = parse_array_suffix st source_type in
  consume st TokSemicolon;
  VarDef { pos; source_type; name; init = None }

and parse_declaration ~is_extern st =
  if is_extern then consume st TokExternKw;
  let pos = cur_pos st in
  let var_ty = parse_type_name st in
  let name = consume_identifier st in
  match st.cur_tok with
  | TokAssign ->
      (* if see =, then expect a init value *)
      parse_var_def_tail st pos var_ty name
  | TokLParen ->
      (* if see (, then expect a function declaration or definition *)
      parse_func_tail st pos var_ty name ~is_extern
  | TokLBracket ->
      (* if see [, then expect array definition *)
      parse_array_def_tail st pos var_ty name
  | TokSemicolon ->
      if is_extern then
        (* todo: support extern variable later *)
        raise (Parse_error (cur_pos st, "extern can only declare a function"));
      (* if see ;, then end the declaration *)
      consume st TokSemicolon;
      VarDef { pos; source_type = var_ty; name; init = None }
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
