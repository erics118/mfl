(** pretty printer for the ast *)

open Ast

let string_of_params l =
  String.concat ", "
    (List.map
       (fun (vt, n) -> Printf.sprintf "%s %s" (string_of_var_type vt) n)
       l)

let rec pp_expr_aux ?(parent_prec = 0) = function
  | IntLiteral (_, n) -> string_of_int n
  | BoolLiteral (_, b) -> string_of_bool b
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
      let e_str =
        match e with
        | BinaryOp _ | UnaryOp _ -> "(" ^ pp_expr_aux e ^ ")"
        | _ -> pp_expr_aux e
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
  | Assign (_, name, value) -> Printf.sprintf "%s = %s" name (pp_expr_aux value)

(** [pp_expr e] renders a value expression into a formatted source string *)
let pp_expr e = pp_expr_aux e

(** indentation helper *)
let pad n = String.make (n * 4) ' '

let rec pp_block_aux ~indent stmts =
  if stmts = [] then "{}"
  else
    let body =
      String.concat "\n"
        (List.map (pp_stmt_aux ~top_level:false ~indent:(indent + 1)) stmts)
    in
    "{\n" ^ body ^ "\n" ^ pad indent ^ "}"

and pp_stmt_aux ?(top_level = true) ?(indent = 0) stmt =
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
    | EmptyStmt _ -> ";"
    | CompoundStmt (_, stmts) ->
        if top_level then
          String.concat "\n"
            (List.map (pp_stmt_aux ~top_level:false ~indent) stmts)
        else pp_block_aux ~indent stmts
    | VarDef { var_type; name; init; _ } ->
        Printf.sprintf "%s %s = %s;"
          (string_of_var_type var_type)
          name (pp_expr_aux init)
    | FuncDef { ret_type; name; params; body; _ } ->
        Printf.sprintf "%s %s(%s) %s"
          (string_of_var_type ret_type)
          name (string_of_params params)
          (pp_block_aux ~indent body)
    | If _ -> pp_body stmt
    | WhileLoop { cond; body; _ } ->
        Printf.sprintf "while (%s) %s" (pp_expr_aux cond) (pp_body body)
    | ForLoop { init; cond; incr; body; _ } ->
        Printf.sprintf "for (%s %s; %s) %s" (pp_stmt_aux init)
          (pp_expr_aux cond) (pp_expr_aux incr) (pp_body body)
  in
  p ^ rest

(** [pp_stmt s] renders a statement into a formatted source string *)
let pp_stmt s = pp_stmt_aux s
