open Ast

let string_of_var_type_list l =
  String.concat ", "
    (List.map (fun (t, n) -> Printf.sprintf "%s %s" (string_of_var_type t) n) l)

let rec pp_value_expr ?(parent_prec = 0) = function
  | IntLiteral n -> string_of_int n
  | BoolLiteral b -> string_of_bool b
  | VarRef name -> name
  | Ternary (e, t, f) ->
      let cond_str =
        (* put in parens *)
        match e with
        | Ternary _ -> "(" ^ pp_value_expr e ^ ")"
        | _ -> pp_value_expr e
      in
      let s =
        Printf.sprintf "%s ? %s : %s" cond_str (pp_value_expr t)
          (pp_value_expr f)
      in
      if parent_prec > 5 then "(" ^ s ^ ")" else s
  | UnaryOp (op, e) ->
      let e_str =
        match e with
        | BinaryOp _ | UnaryOp _ -> "(" ^ pp_value_expr e ^ ")"
        | _ -> pp_value_expr e
      in
      string_of_uop op ^ e_str
  | BinaryOp (op, lhs, rhs) ->
      let prec = precedence op in
      let lhs_str = pp_value_expr ~parent_prec:prec lhs in
      let rhs_prec =
        match op with
        | Sub | Div | Mod -> prec + 1
        | _ -> prec
      in
      let rhs_str = pp_value_expr ~parent_prec:rhs_prec rhs in
      let s = Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str in
      if prec < parent_prec then "(" ^ s ^ ")" else s
  | FuncCall { name; args } ->
      let args_str = String.concat ", " (List.map pp_value_expr args) in
      Printf.sprintf "%s(%s)" name args_str
  | _ -> failwith "not a value expr" [@coverage off]

let pad n = String.make (n * 4) ' '

let rec pp_block ~indent stmts =
  if stmts = [] then "{}"
  else
    let body =
      String.concat "\n"
        (List.map (pp_expr_ ~top_level:false ~indent:(indent + 1)) stmts)
    in
    "{\n" ^ body ^ "\n}"

(** private [pp_expr_] function, with optional args *)
and pp_expr_ ?(top_level = true) ?(indent = 0) = function
  | Statement e -> pad indent ^ pp_value_expr e ^ ";"
  | ReturnStmt None -> pad indent ^ "return;"
  | ReturnStmt (Some e) -> pad indent ^ "return " ^ pp_value_expr e ^ ";"
  | EmptyStmt -> pad indent ^ ";"
  | CompoundStmt statements ->
      if top_level then
        String.concat "\n"
          (List.map (pp_expr_ ~top_level:false ~indent) statements)
      else pp_block ~indent statements
  | VarDef { var_type; name; init } ->
      pad indent
      ^ Printf.sprintf "%s %s = %s;"
          (string_of_var_type var_type)
          name (pp_value_expr init)
  | FuncDef { ret_type; name; params; body } ->
      pad indent
      ^ Printf.sprintf "%s %s(%s) %s"
          (string_of_var_type ret_type)
          name
          (string_of_var_type_list params)
          (pp_block ~indent body)
  | e -> pp_value_expr e

(* public function without the optional args *)
let pp_expr e = pp_expr_ e
