open Ast

type type_error =
  | UnknownType of string
  | UnboundVariable of string
  | UnboundFunction of string
  | ArityMismatch of string * int * int (* name, expected, got *)
  | BinaryTypeMismatch of op * typ * typ
  | UnaryTypeMismatch of uop * typ
  | TypeMismatch of typ * typ (* expected, got *)
  | CondNotBool of typ

exception Type_error of pos * type_error

let string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"

let string_of_type_error = function
  | UnknownType name -> Printf.sprintf "unknown type '%s'" name
  | UnboundVariable x -> Printf.sprintf "unbound variable '%s'" x
  | UnboundFunction f -> Printf.sprintf "unbound function '%s'" f
  | ArityMismatch (f, expected, got) ->
      Printf.sprintf "'%s' expects %d argument(s) but got %d" f expected got
  | BinaryTypeMismatch (op, lt, rt) ->
      Printf.sprintf "operator '%s': type mismatch between '%s' and '%s'"
        (string_of_op op) (string_of_typ lt) (string_of_typ rt)
  | UnaryTypeMismatch (op, t) ->
      Printf.sprintf "operator '%s': invalid operand type '%s'"
        (string_of_uop op) (string_of_typ t)
  | TypeMismatch (expected, got) ->
      Printf.sprintf "expected type '%s' but got '%s'" (string_of_typ expected)
        (string_of_typ got)
  | CondNotBool t ->
      Printf.sprintf "condition must be 'bool' but got '%s'" (string_of_typ t)

type func_sig = {
  params : typ list;
  ret : typ;
}

type env = {
  vars : (string, typ) Hashtbl.t;
  funcs : (string, func_sig) Hashtbl.t;
  return_typ : typ option;
}

let typ_of_var_type pos = function
  | VarType "int" -> Int
  | VarType "bool" -> Bool
  | VarType "void" -> Void
  | VarType name -> raise (Type_error (pos, UnknownType name))

let expr_typ : checked expr -> typ = function
  | IntLiteral (ann, _)
  | BoolLiteral (ann, _)
  | VarRef (ann, _)
  | BinaryOp (ann, _, _, _)
  | UnaryOp (ann, _, _)
  | Ternary (ann, _, _, _)
  | FuncCall (ann, _, _)
  | Assign (ann, _, _) -> typ_of ann

let check_binary pos op lt rt =
  let err () = raise (Type_error (pos, BinaryTypeMismatch (op, lt, rt))) in
  match op with
  | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | LShift | RShift ->
      if lt = Int && rt = Int then Int else err ()
  | Less | Leq | Greater | Geq -> if lt = Int && rt = Int then Bool else err ()
  | And | Or -> if lt = Bool && rt = Bool then Bool else err ()
  | Equal | Neq -> if lt = rt then Bool else err ()

let check_unary pos op t =
  let err () = raise (Type_error (pos, UnaryTypeMismatch (op, t))) in
  match op with
  | Not -> if t = Bool then Bool else err ()
  | Neg | Compl -> if t = Int then Int else err ()

let check_ternary pos cond_t then_t else_t =
  if cond_t <> Bool then raise (Type_error (pos, CondNotBool cond_t));
  if then_t <> else_t then
    raise (Type_error (pos, TypeMismatch (then_t, else_t)));
  then_t

let rec typecheck_expr env expr =
  match expr with
  | IntLiteral (ann, n) -> typecheck_int_lit ann n
  | BoolLiteral (ann, b) -> typecheck_bool_lit ann b
  | BinaryOp (ann, op, lhs, rhs) -> typecheck_binary_op env ann op lhs rhs
  | VarRef (ann, x) -> typecheck_var_ref env ann x
  | UnaryOp (ann, op, e) -> typecheck_unary_op env ann op e
  | Ternary (ann, cond, t, e) -> typecheck_ternary_op env ann cond t e
  | FuncCall (ann, f, args) -> typecheck_func_call env ann f args
  | Assign (ann, x, e) -> typecheck_assign env ann x e

and typecheck_int_lit ann n = IntLiteral (Checked (pos_of ann, Int), n)
and typecheck_bool_lit ann b = BoolLiteral (Checked (pos_of ann, Bool), b)

and typecheck_binary_op env ann op lhs rhs =
  let pos = pos_of ann in
  let lhs' = typecheck_expr env lhs in
  let rhs' = typecheck_expr env rhs in
  let t = check_binary pos op (expr_typ lhs') (expr_typ rhs') in
  BinaryOp (Checked (pos, t), op, lhs', rhs')

and typecheck_var_ref env ann x =
  let pos = pos_of ann in
  let t =
    match Hashtbl.find_opt env.vars x with
    | Some t -> t
    | None -> raise (Type_error (pos, UnboundVariable x))
  in
  VarRef (Checked (pos, t), x)

and typecheck_unary_op env ann op e =
  let pos = pos_of ann in
  let e' = typecheck_expr env e in
  let t = check_unary pos op (expr_typ e') in
  UnaryOp (Checked (pos, t), op, e')

and typecheck_ternary_op env ann cond then_e else_e =
  let pos = pos_of ann in
  let cond' = typecheck_expr env cond in
  let then_e' = typecheck_expr env then_e in
  let else_e' = typecheck_expr env else_e in
  let t =
    check_ternary pos (expr_typ cond') (expr_typ then_e') (expr_typ else_e')
  in
  Ternary (Checked (pos, t), cond', then_e', else_e')

and typecheck_func_call env ann f args =
  let pos = pos_of ann in
  let sig_ =
    match Hashtbl.find_opt env.funcs f with
    | Some s -> s
    | None -> raise (Type_error (pos, UnboundFunction f))
  in
  let expected = List.length sig_.params and got = List.length args in
  if expected <> got then
    raise (Type_error (pos, ArityMismatch (f, expected, got)));
  let args' =
    List.map2
      (fun param_t arg ->
        let arg' = typecheck_expr env arg in
        let at = expr_typ arg' in
        if at <> param_t then
          raise (Type_error (pos, TypeMismatch (param_t, at)));
        arg')
      sig_.params args
  in
  FuncCall (Checked (pos, sig_.ret), f, args')

and typecheck_assign env ann x e =
  let pos = pos_of ann in
  let t =
    match Hashtbl.find_opt env.vars x with
    | Some t -> t
    | None -> raise (Type_error (pos, UnboundVariable x))
  in
  let e' = typecheck_expr env e in
  let et = expr_typ e' in
  if et <> t then raise (Type_error (pos, TypeMismatch (t, et)));
  Assign (Checked (pos, t), x, e')
