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
  | ReturnOutsideFunction

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
  | ReturnOutsideFunction -> "return statement outside of a function"

type func_sig = {
  params : typ list;
  ret : typ;
}

type env = {
  (* head is innermost scope. push when entering a new block, discard on exit *)
  vars : (string, typ) Hashtbl.t list;
  funcs : (string, func_sig) Hashtbl.t;
  return_typ : typ option;
}

let lookup_var env x =
  List.find_map (fun scope -> Hashtbl.find_opt scope x) env.vars

let define_var env name t =
  match env.vars with
  | scope :: _ -> Hashtbl.replace scope name t
  | [] -> failwith "empty scope stack"

let push_scope env = { env with vars = Hashtbl.create 8 :: env.vars }

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

let rec typecheck_expr env (expr : parsed expr) : checked expr =
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
  let lhs = typecheck_expr env lhs in
  let rhs = typecheck_expr env rhs in
  (* ensure lhs and rhs have the correct type for the operator *)
  let t = check_binary pos op (expr_typ lhs) (expr_typ rhs) in
  BinaryOp (Checked (pos, t), op, lhs, rhs)

and typecheck_var_ref env ann x =
  let pos = pos_of ann in
  (* error if the variable doesnt exist *)
  let t =
    match lookup_var env x with
    | Some t -> t
    | None -> raise (Type_error (pos, UnboundVariable x))
  in
  VarRef (Checked (pos, t), x)

and typecheck_unary_op env ann op e =
  let pos = pos_of ann in
  let e = typecheck_expr env e in
  (* ensure lhs has the correct type for the operator *)
  let t = check_unary pos op (expr_typ e) in
  UnaryOp (Checked (pos, t), op, e)

and typecheck_ternary_op env ann cond then_e else_e =
  let pos = pos_of ann in
  let cond = typecheck_expr env cond in
  let then_e = typecheck_expr env then_e in
  let else_e = typecheck_expr env else_e in
  (* ensure cond is a bool, and then_e and else_e have the same type *)
  let t =
    check_ternary pos (expr_typ cond) (expr_typ then_e) (expr_typ else_e)
  in
  Ternary (Checked (pos, t), cond, then_e, else_e)

and typecheck_func_call env ann f args =
  let pos = pos_of ann in
  (* ensure function exists *)
  let sig_ =
    match Hashtbl.find_opt env.funcs f with
    | Some s -> s
    | None -> raise (Type_error (pos, UnboundFunction f))
  in
  (* ensure correct number of parameters *)
  let expected = List.length sig_.params and got = List.length args in
  if expected <> got then
    raise (Type_error (pos, ArityMismatch (f, expected, got)));
  (* map2, ensuring each param has the right type *)
  let args =
    List.map2
      (fun param_t arg ->
        let arg = typecheck_expr env arg in
        let at = expr_typ arg in
        if at <> param_t then
          raise (Type_error (pos, TypeMismatch (param_t, at)));
        arg)
      sig_.params args
  in
  FuncCall (Checked (pos, sig_.ret), f, args)

and typecheck_stmt env (stmt : parsed stmt) : checked stmt =
  match stmt with
  | EmptyStmt pos ->
      (* trivial *)
      EmptyStmt pos
  | ExprStmt (pos, e) ->
      (* we can just check the inner expr *)
      ExprStmt (pos, typecheck_expr env e)
  | ReturnStmt (pos, e) -> typecheck_return env pos e
  | CompoundStmt (pos, stmts) ->
      (* create a new environment *)
      let env = push_scope env in
      (* we can just check each statement *)
      CompoundStmt (pos, List.map (typecheck_stmt env) stmts)
  | VarDef { pos; var_type; name; init } ->
      typecheck_var_def env pos var_type name init
  | FuncDef { pos; ret_type; name; params; body } ->
      typecheck_func_def env pos ret_type name params body
  | If { pos; cond; then_body; else_body } ->
      typecheck_if env pos cond then_body else_body
  | WhileLoop { pos; cond; body } -> typecheck_while env pos cond body
  | ForLoop { pos; init; cond; incr; body } ->
      typecheck_for env pos init cond incr body

and typecheck_return env pos e =
  (* if return type is None, that means we are returning outside a function *)
  let ret =
    match env.return_typ with
    | None -> raise (Type_error (pos, ReturnOutsideFunction))
    | Some t -> t
  in
  match e with
  | None ->
      (* if the return value is also void, good *)
      if ret <> Void then raise (Type_error (pos, TypeMismatch (ret, Void)));
      ReturnStmt (pos, None)
  | Some e ->
      (* ensure that return value is equal to ret, the correct return type *)
      let e = typecheck_expr env e in
      let t = expr_typ e in
      if t <> ret then raise (Type_error (pos, TypeMismatch (ret, t)));
      ReturnStmt (pos, Some e)

and typecheck_var_def env pos var_type name init =
  let var_t = typ_of_var_type pos var_type in
  let init = typecheck_expr env init in
  let init_t = expr_typ init in
  (* ensure init has the right type *)
  if var_t <> init_t then raise (Type_error (pos, TypeMismatch (var_t, init_t)));
  define_var env name var_t;
  VarDef { pos; var_type; name; init }

and typecheck_func_def env pos ret_type name params body =
  let ret_t = typ_of_var_type pos ret_type in
  (* add function to env first *)
  Hashtbl.replace env.funcs name
    {
      params = List.map (fun (vt, _) -> typ_of_var_type pos vt) params;
      ret = ret_t;
    };
  let fn_env =
    { vars = [ Hashtbl.create 8 ]; funcs = env.funcs; return_typ = Some ret_t }
  in
  (* then we add the variables to the env *)
  List.iter
    (fun (vt, pname) -> define_var fn_env pname (typ_of_var_type pos vt))
    params;
  let body = List.map (typecheck_stmt fn_env) body in
  FuncDef { pos; ret_type; name; params; body }

and typecheck_if env pos cond then_body else_body =
  let cond = typecheck_expr env cond in
  (* recursively typecheck then_body *)
  let then_body = typecheck_stmt env then_body in
  (* recursively typecheck else_body is Some *)
  let else_body = Option.map (typecheck_stmt env) else_body in
  let cond_t = expr_typ cond in
  (* ensure cond is Bool *)
  if cond_t <> Bool then raise (Type_error (pos, CondNotBool cond_t));
  If { pos; cond; then_body; else_body }

and typecheck_while env pos cond body =
  let cond = typecheck_expr env cond in
  let body = typecheck_stmt env body in
  let cond_t = expr_typ cond in
  (* ensure cond is Bool *)
  if cond_t <> Bool then raise (Type_error (pos, CondNotBool cond_t));
  WhileLoop { pos; cond; body }

and typecheck_for env pos init cond incr body =
  let env = push_scope env in
  let init = typecheck_stmt env init in
  let cond = typecheck_expr env cond in
  let incr = typecheck_expr env incr in
  let body = typecheck_stmt env body in
  let cond_t = expr_typ cond in
  (* ensure cond is Bool *)
  if cond_t <> Bool then raise (Type_error (pos, CondNotBool cond_t));
  (* incr can be anything, we dont need to check its type *)
  ForLoop { pos; init; cond; incr; body }

and typecheck_assign env ann x e =
  let pos = pos_of ann in
  let t =
    match lookup_var env x with
    | Some t -> t
    | None -> raise (Type_error (pos, UnboundVariable x))
  in
  let e = typecheck_expr env e in
  let et = expr_typ e in
  if et <> t then raise (Type_error (pos, TypeMismatch (t, et)));
  Assign (Checked (pos, t), x, e)

let typecheck_program stmts =
  let funcs = Hashtbl.create 8 in
  (* "stdlib" functions *)
  Hashtbl.replace funcs "printint" { params = [ Int ]; ret = Void };
  Hashtbl.replace funcs "printbool" { params = [ Bool ]; ret = Void };
  let env = { vars = [ Hashtbl.create 8 ]; funcs; return_typ = None } in
  List.map (typecheck_stmt env) stmts
