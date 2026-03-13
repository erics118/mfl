(* binary operators *)
type op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor

(* unary operators *)
type uop =
  | Neg
  | Not

type var_type = VarType of string

let string_of_var_type (VarType name) = name

(* expressions *)
type expr =
  | IntLiteral of int
  | BoolLiteral of bool
  | VarRef of string
  | BinaryOp of op * expr * expr (* binary operators *)
  | UnaryOp of uop * expr (* unary operators *)
  | Statement of expr (* complete statement (currently just an expr) *)
  | ReturnStmt of expr option
  | EmptyStmt
  | CompoundStmt of expr list (* sequence of statements surrounded by braces *)
  | Ternary of expr * expr * expr (* ternary operator *)
  (* typed variable definition: <type> <name> = <expr>; *)
  | VarDef of {
      var_type : var_type;
      name : string;
      init : expr;
    }
  | FuncDef of {
      ret_type : var_type;
      name : string;
      params : (var_type * string) list;
      body : expr;
    }
  | FuncCall of {
      name : string;
      args : expr list;
    }

let precedence = function
  | Or -> 10
  | And -> 20
  | Equal | Neq -> 30
  | Less | Leq | Greater | Geq -> 40
  | BitOr -> 50
  | BitXor -> 60
  | BitAnd -> 70
  | Add | Sub -> 80
  | Mul | Div | Mod -> 90

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"

let op_of_string_opt = function
  | "+" -> Some Add
  | "-" -> Some Sub
  | "*" -> Some Mul
  | "/" -> Some Div
  | "%" -> Some Mod
  | "<" -> Some Less
  | ">" -> Some Greater
  | "==" -> Some Equal
  | "!=" -> Some Neq
  | "<=" -> Some Leq
  | ">=" -> Some Geq
  | "&&" -> Some And
  | "||" -> Some Or
  | "&" -> Some BitAnd
  | "|" -> Some BitOr
  | "^" -> Some BitXor
  | _ -> None [@coverage off]

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

let string_of_var_type_list l =
  String.concat ", "
    (List.map (fun (t, n) -> Printf.sprintf "%s %s" (string_of_var_type t) n) l)

let rec pp_expr ?(parent_prec = 0) ?(top_level = true) = function
  | IntLiteral n -> string_of_int n
  | BoolLiteral b -> string_of_bool b
  | VarRef name -> name
  | UnaryOp (op, e) ->
      let e_str =
        match e with
        | BinaryOp _ | UnaryOp _ -> "(" ^ pp_expr ~top_level:false e ^ ")"
        | _ -> pp_expr ~top_level:false e
      in
      string_of_uop op ^ e_str
  | BinaryOp (op, lhs, rhs) ->
      let prec = precedence op in
      let lhs_str = pp_expr ~parent_prec:prec ~top_level:false lhs in
      let rhs_prec =
        match op with
        | Sub | Div | Mod -> prec + 1
        | _ -> prec
      in
      let rhs_str = pp_expr ~parent_prec:rhs_prec ~top_level:false rhs in
      let s = Printf.sprintf "%s %s %s" lhs_str (string_of_op op) rhs_str in
      if prec < parent_prec then "(" ^ s ^ ")" else s
  | Statement e -> pp_expr ~top_level:false e ^ ";"
  | ReturnStmt None -> "return;"
  | ReturnStmt (Some e) -> "return " ^ pp_expr ~top_level:false e ^ ";"
  | EmptyStmt -> ";"
  | CompoundStmt statements ->
      let body =
        String.concat "\n" (List.map (pp_expr ~top_level:false) statements)
      in
      if top_level then body else "{" ^ body ^ "}"
  | Ternary (e, t, f) ->
      let cond_str =
        (* put in parens *)
        match e with
        | Ternary _ -> "(" ^ pp_expr ~top_level:false e ^ ")"
        | _ -> pp_expr ~top_level:false e
      in
      let s =
        Printf.sprintf "%s ? %s : %s" cond_str
          (pp_expr ~top_level:false t)
          (pp_expr ~top_level:false f)
      in
      if parent_prec > 5 then "(" ^ s ^ ")" else s
  | VarDef { var_type; name; init } ->
      Printf.sprintf "%s %s = %s;"
        (string_of_var_type var_type)
        name
        (pp_expr ~top_level:false init)
  | FuncDef { ret_type; name; params; body } ->
      Printf.sprintf "%s %s(%s) %s"
        (string_of_var_type ret_type)
        name
        (string_of_var_type_list params)
        (pp_expr ~top_level:false body)
  | FuncCall { name; args } ->
      let args_str =
        String.concat ", " (List.map (pp_expr ~top_level:false) args)
      in
      Printf.sprintf "%s(%s)" name args_str
