open Ast

let string_of_var_type_list l =
  String.concat ", "
    (List.map (fun (t, n) -> Printf.sprintf "%s %s" (string_of_var_type t) n) l)

let rec pp_expr_ ?(parent_prec = 0) = function
  | IntLiteral n -> string_of_int n
  | BoolLiteral b -> string_of_bool b
  | VarRef name -> name
  | Ternary (e, t, f) ->
      let cond_str =
        match e with
        | Ternary _ -> "(" ^ pp_expr_ e ^ ")"
        | _ -> pp_expr_ e
      in
      let s =
        Printf.sprintf "%s ? %s : %s" cond_str (pp_expr_ t) (pp_expr_ f)
      in
      if parent_prec > 5 then "(" ^ s ^ ")" else s
  | UnaryOp (op, e) ->
      let e_str =
        match e with
        | BinaryOp _ | UnaryOp _ -> "(" ^ pp_expr_ e ^ ")"
        | _ -> pp_expr_ e
      in
      string_of_uop op ^ e_str
  | BinaryOp (op, lhs, rhs) ->
      let prec = precedence op in
      let lhs_str = pp_expr_ ~parent_prec:prec lhs in
      let rhs_prec =
        match op with
        | Sub | Div | Mod -> prec + 1
        | _ -> prec
      in
      let rhs_str = pp_expr_ ~parent_prec:rhs_prec rhs in
      let s = Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str in
      if prec < parent_prec then "(" ^ s ^ ")" else s
  | FuncCall { name; args } ->
      let args_str = String.concat ", " (List.map pp_expr_ args) in
      Printf.sprintf "%s(%s)" name args_str

let pp_expr e = pp_expr_ e

(** indentation helper *)
let pad n = String.make (n * 4) ' '

let rec pp_block_ ~indent stmts =
  if stmts = [] then "{}"
  else
    let body =
      String.concat "\n"
        (List.map
           (pp_stmt_internal ~top_level:false ~indent:(indent + 1))
           stmts)
    in
    "{\n" ^ body ^ "\n" ^ pad indent ^ "}"

and pp_stmt_internal ?(top_level = true) ?(indent = 0) stmt =
  let p = pad indent in
  let rec pp_body stmt =
    match stmt with
    | CompoundStmt stmts -> pp_block_ ~indent stmts
    | If { cond; then_body; else_body } ->
        (match else_body with
         | Some e ->
             Printf.sprintf "if (%s) %s else %s" (pp_expr_ cond)
               (pp_body then_body) (pp_body e)
         | None ->
             Printf.sprintf "if (%s) %s" (pp_expr_ cond) (pp_body then_body))
    | s -> pp_block_ ~indent [ s ]
  in
  let pp_for_init = function
    | VarDef { var_type; name; init } ->
        Printf.sprintf "%s %s = %s" (string_of_var_type var_type) name (pp_expr_ init)
    | ExprStmt e -> pp_expr_ e
    | EmptyStmt -> ""
    | _ -> assert false
  in
  match stmt with
  | ExprStmt e -> p ^ pp_expr_ e ^ ";"
  | ReturnStmt None -> p ^ "return;"
  | ReturnStmt (Some e) -> p ^ "return " ^ pp_expr_ e ^ ";"
  | EmptyStmt -> p ^ ";"
  | CompoundStmt stmts ->
      if top_level then
        String.concat "\n"
          (List.map (pp_stmt_internal ~top_level:false ~indent) stmts)
      else pp_block_ ~indent stmts
  | VarDef { var_type; name; init } ->
      p ^ Printf.sprintf "%s %s = %s;" (string_of_var_type var_type) name (pp_expr_ init)
  | FuncDef { ret_type; name; params; body } ->
      p ^ Printf.sprintf "%s %s(%s) %s"
            (string_of_var_type ret_type)
            name
            (string_of_var_type_list params)
            (pp_block_ ~indent body)
  | If { cond; then_body; else_body } ->
      p ^ (match else_body with
           | Some e ->
               Printf.sprintf "if (%s) %s else %s" (pp_expr_ cond)
                 (pp_body then_body) (pp_body e)
           | None ->
               Printf.sprintf "if (%s) %s" (pp_expr_ cond) (pp_body then_body))
  | WhileLoop { cond; body } ->
      p ^ Printf.sprintf "while (%s) %s" (pp_expr_ cond) (pp_body body)
  | ForLoop { init; cond; incr; body } ->
      p ^ Printf.sprintf "for (%s; %s; %s) %s"
            (pp_for_init init) (pp_expr_ cond) (pp_expr_ incr) (pp_body body)

let pp_stmt s = pp_stmt_internal s
