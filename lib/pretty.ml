(** pretty printer for the ast *)

open Ast

let string_of_params l =
  String.concat ", "
    (List.map
       (fun (vt, n) -> Printf.sprintf "%s %s" (string_of_var_type vt) n)
       l)

let formatted_string_of_char c =
  let s =
    match c with
    (* simple escape sequence *)
    | 0 -> "\\0"
    | 7 -> "\\a"
    | 8 -> "\\b"
    | 9 -> "\\t"
    | 10 -> "\\n"
    | 11 -> "\\v"
    | 12 -> "\\f"
    | 13 -> "\\r"
    | 92 -> "\\\\"
    | 39 -> "\\'"
    (* normal chars *)
    | n when n >= 32 && n <= 126 -> String.make 1 (Char.chr n)
    (* hexadecimal escape sequence *)
    | n -> Printf.sprintf "\\x%02x" n
  in
  Printf.sprintf "'%s'" s

let rec pp_expr_aux : type a. ?parent_prec:int -> a expr -> string =
 fun ?(parent_prec = 0) -> function
  | IntLiteral (_, n) -> string_of_int n
  | BoolLiteral (_, b) -> string_of_bool b
  | CharLiteral (_, c) -> formatted_string_of_char c
  | VarRef (_, name) -> name
  | Ternary (_, cond, then_e, else_e) ->
      let cond_str =
        match cond with
        | Ternary _ -> "(" ^ pp_expr_aux cond ^ ")"
        | _ -> pp_expr_aux cond
      in
      let s =
        Printf.sprintf "%s ? %s : %s" cond_str (pp_expr_aux then_e)
          (pp_expr_aux else_e)
      in
      if parent_prec > 5 then "(" ^ s ^ ")" else s
  | UnaryOp (_, op, e) ->
      let needs_parens =
        (* handle needing parens only when absolutely necessary *)
        match (op, e) with
        (* bin ops, assign, ternary always do *)
        | _, (BinaryOp _ | Assign _ | Ternary _) -> true
        (* prevent Neg Neg to turn into --, and Neg PreDec into --- *)
        | Neg, (UnaryOp (_, Neg, _) | PreDec _) -> true
        (* prevent AddrOf AddrOf from turning into && *)
        | AddrOf, UnaryOp (_, AddrOf, _) -> true
        | _ -> false
      in
      let e_str =
        if needs_parens then "(" ^ pp_expr_aux e ^ ")" else pp_expr_aux e
      in
      string_of_uop op ^ e_str
  | BinaryOp (_, op, lhs, rhs) ->
      let prec = precedence op in
      let lhs_str = pp_expr_aux ~parent_prec:prec lhs in
      let rhs_prec =
        match op with
        | Sub | Div | Mod -> prec + 1
        | _ -> prec
      in
      let rhs_str = pp_expr_aux ~parent_prec:rhs_prec rhs in
      let s = Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str in
      if prec < parent_prec then "(" ^ s ^ ")" else s
  | FuncCall (_, name, args) ->
      let args_str = String.concat ", " (List.map pp_expr_aux args) in
      Printf.sprintf "%s(%s)" name args_str
  | Assign (_, e, value) ->
      Printf.sprintf "%s = %s" (pp_expr_aux e) (pp_expr_aux value)
  | PreInc (_, e) -> "++" ^ pp_expr_aux e
  | PreDec (_, e) -> "--" ^ pp_expr_aux e
  (* postfix binds tighter than any prefix operator, so any prefix-op operand
     needs parens around it *)
  | PostInc (_, e) ->
      let s = pp_expr_aux e in
      let s =
        match e with
        | PreInc _ | PreDec _ | UnaryOp _ -> "(" ^ s ^ ")"
        | _ -> s
      in
      s ^ "++"
  | PostDec (_, e) ->
      let s = pp_expr_aux e in
      let s =
        match e with
        | PreInc _ | PreDec _ | UnaryOp _ -> "(" ^ s ^ ")"
        | _ -> s
      in
      s ^ "--"
  | Subscript (_, a, i) ->
      (* todo: this might need parens *)
      Printf.sprintf "%s[%s]" (pp_expr_aux a) (pp_expr_aux i)
  | Cast (_, ty, e) ->
      let e_str =
        match e with
        | BinaryOp _ | Ternary _ | Assign _ -> "(" ^ pp_expr_aux e ^ ")"
        | _ -> pp_expr_aux e
      in
      Printf.sprintf "(%s)%s" (string_of_var_type ty) e_str
  | ImplicitCast (_, _, e) ->
      (* implicit casts don't need to be shown *)
      pp_expr_aux e
  | SizeofExpr (_, e) -> "sizeof (" ^ pp_expr_aux e ^ ")"
  | SizeofType (_, t) -> "sizeof " ^ string_of_var_type t

(* print an 'a expr option, handling spacing, for use within a for loop *)
let pp_expr_aux_opt = function
  | None -> ""
  | Some e -> " " ^ pp_expr_aux e

(** [pp_expr e] renders a value expression into a formatted source string *)
let pp_expr : type a. a expr -> string = fun e -> pp_expr_aux e

(** indentation helper *)
let pad n = String.make (n * 4) ' '

let rec pp_block_aux : type a. indent:int -> a stmt list -> string =
 fun ~indent stmts ->
  if stmts = [] then "{}"
  else
    let body =
      String.concat "\n"
        (List.map (pp_stmt_aux ~top_level:false ~indent:(indent + 1)) stmts)
    in
    "{\n" ^ body ^ "\n" ^ pad indent ^ "}"

and pp_stmt_aux : type a. ?top_level:bool -> ?indent:int -> a stmt -> string =
 fun ?(top_level = true) ?(indent = 0) stmt ->
  let p = pad indent in
  let rec pp_body stmt =
    match stmt with
    | CompoundStmt (_, stmts) -> pp_block_aux ~indent stmts
    | If { cond; then_body; else_body; _ } -> begin
        match else_body with
        | Some s ->
            Printf.sprintf "if (%s) %s else %s" (pp_expr_aux cond)
              (pp_body then_body) (pp_body s)
        | None ->
            Printf.sprintf "if (%s) %s" (pp_expr_aux cond) (pp_body then_body)
      end
    | s -> pp_block_aux ~indent [ s ]
  in
  let rest =
    match stmt with
    | ExprStmt (_, e) -> pp_expr_aux e ^ ";"
    | ReturnStmt (_, None) -> "return;"
    | ReturnStmt (_, Some e) -> "return " ^ pp_expr_aux e ^ ";"
    | BreakStmt _ -> "break;"
    | ContinueStmt _ -> "continue;"
    | EmptyStmt _ -> ";"
    | CompoundStmt (_, stmts) ->
        if top_level then
          String.concat "\n"
            (List.map (pp_stmt_aux ~top_level:false ~indent) stmts)
        else pp_block_aux ~indent stmts
    | VarDef { var_type; name; init; _ } -> begin
        (* array declarators put [n] after the name, not the type, so we need to
           specifically handle them *)
        let decl_str =
          match var_type with
          | VArray (t, sz) ->
              Printf.sprintf "%s %s[%d]" (string_of_var_type t) name sz
          | _ -> Printf.sprintf "%s %s" (string_of_var_type var_type) name
        in
        match init with
        | None -> decl_str ^ ";"
        | Some init -> Printf.sprintf "%s = %s;" decl_str (pp_expr_aux init)
      end
    | Typedef { existing_type; alias; _ } ->
        Printf.sprintf "typedef %s %s;" (string_of_var_type existing_type) alias
    | FuncDef { ret_type; name; params; body; _ } ->
        Printf.sprintf "%s %s(%s) %s"
          (string_of_var_type ret_type)
          name (string_of_params params)
          (pp_block_aux ~indent body)
    | If _ -> pp_body stmt
    | WhileLoop { cond; body; _ } ->
        Printf.sprintf "while (%s) %s" (pp_expr_aux cond) (pp_body body)
    | ForLoop { init; cond; incr; body; _ } ->
        Printf.sprintf "for (%s%s;%s) %s" (pp_stmt_aux init)
          (pp_expr_aux_opt cond) (pp_expr_aux_opt incr) (pp_body body)
    | DoWhileLoop { body; cond; _ } ->
        Printf.sprintf "do %s while (%s);" (pp_body body) (pp_expr_aux cond)
  in
  p ^ rest

(** [pp_stmt s] renders a statement into a formatted source string *)
let pp_stmt : type a. a stmt -> string = fun s -> pp_stmt_aux s
