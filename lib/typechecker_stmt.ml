open Ast
open Typechecker_error
open Typechecker_env
open Typechecker_expr
open Typechecker_types

(** this is true if execution may continue past it. we need this to determine of
    a function has a missing return statement. "fall through" means can exit the
    block without a return statement *)
let rec stmt_can_fall_through = function
  | ReturnStmt _ -> false
  | CompoundStmt (_, stmts) -> stmts_can_fall_through stmts
  | If { then_body; else_body = Some else_body; _ } ->
      stmt_can_fall_through then_body || stmt_can_fall_through else_body
  | If { else_body = None; _ } -> true
  | ExprStmt _
  | EmptyStmt _
  | VarDef _
  | Typedef _
  | StructDef _
  | FuncDecl _
  | FuncDef _
  | BreakStmt _
  | ContinueStmt _
  | WhileLoop _
  | ForLoop _
  | DoWhileLoop _ -> true

(** goes through a list of statements until it finds a statement that cannot
    fall through. *)
and stmts_can_fall_through = function
  | [] -> true
  | stmt :: rest ->
      if stmt_can_fall_through stmt then stmts_can_fall_through rest else false

let rec typecheck_stmt (env : env) (stmt : parsed stmt) : checked stmt =
  match stmt with
  | EmptyStmt pos ->
      (* trivial *)
      EmptyStmt pos
  | BreakStmt pos ->
      if not env.in_loop then raise (Type_error (pos, BreakOutsideLoop));
      BreakStmt pos
  | ContinueStmt pos ->
      if not env.in_loop then raise (Type_error (pos, ContinueOutsideLoop));
      ContinueStmt pos
  | ExprStmt (pos, e) ->
      (* we can just check the inner expr *)
      ExprStmt (pos, typecheck_expr env e)
  | ReturnStmt (pos, e) -> typecheck_return env pos e
  | CompoundStmt (pos, stmts) ->
      (* create a new environment *)
      let env = push_scope env in
      (* we can just check each statement *)
      CompoundStmt (pos, List.map (typecheck_stmt env) stmts)
  | Typedef { pos; struct_def; existing_type; alias } ->
      (* if there's an inline struct def, register it first *)
      (match struct_def with
      | None -> ()
      | Some (tag, fields) ->
          let resolved =
            List.map
              (fun (vt, fname) -> (fname, resolve_source_type env pos vt))
              fields
          in
          define_struct env tag resolved);
      let existing_type =
        source_type_of_typ (resolve_source_type env pos existing_type)
      in
      define_typedef env alias existing_type;
      Typedef { pos; struct_def; existing_type; alias }
  | StructDef { pos; tag; fields; var_name } ->
      typecheck_struct_def env pos tag fields var_name
  | FuncDecl { pos; ret_type; name; params; is_extern; is_variadic } ->
      typecheck_func_decl env pos ret_type name params is_extern is_variadic
  | VarDef { pos; source_type; name; init } ->
      typecheck_var_def env pos source_type name init
  | FuncDef { pos; ret_type; name; params; is_variadic; body } ->
      if is_variadic then
        failwith "variadic function definitions not implemented yet";
      typecheck_func_def env pos ret_type name params body
  | If { pos; cond; then_body; else_body } ->
      typecheck_if env pos cond then_body else_body
  | WhileLoop { pos; cond; body } -> typecheck_while env pos cond body
  | ForLoop { pos; init; cond; incr; body } ->
      typecheck_for env pos init cond incr body
  | DoWhileLoop { pos; body; cond } -> typecheck_do_while env pos body cond

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
      let e = cast_expr pos ret e in
      let t = expr_typ e in
      if t <> ret then raise (Type_error (pos, TypeMismatch (ret, t)));
      ReturnStmt (pos, Some e)

and typecheck_var_def env pos source_type name init =
  let var_t = resolve_source_type env pos source_type in
  (* if init exists, we check its type to ensure it is valid *)
  let init =
    match init with
    | None -> None
    | Some init -> begin
        let init = typecheck_expr env init in
        let init = cast_expr pos var_t init in
        let init_t = expr_typ init in
        (* ensure init has the right type *)
        if var_t <> init_t then
          raise (Type_error (pos, TypeMismatch (var_t, init_t)));
        Some init
      end
  in
  define_var env name var_t;
  VarDef { pos; source_type = source_type_of_typ var_t; name; init }

and typecheck_struct_def env pos tag fields var_name =
  (* resolve each field type and register the struct *)
  let resolved =
    List.map (fun (vt, fname) -> (fname, resolve_source_type env pos vt)) fields
  in
  define_struct env tag resolved;
  (* if var_name is given, also define a variable of this struct type *)
  (match var_name with
  | None -> ()
  | Some name -> define_var env name (Struct tag));
  StructDef { pos; tag; fields; var_name }

and normalize_param_type = function
  | Array (t, _) -> Ptr t
  | t -> t

(* normalizes a list of parameters, ie typedefs, ptr, array *)
and normalize_params env pos params =
  List.map
    (fun (vt, pname) ->
      ( source_type_of_typ
          (normalize_param_type (resolve_source_type env pos vt)),
        pname ))
    params

(* register the function signature *)
and register_func_sig env pos ret_type name params is_variadic =
  let ret_t = resolve_source_type env pos ret_type in
  if is_variadic then failwith "variadic typechecking not implemented yet"
  else
    let params = normalize_params env pos params in
    Hashtbl.replace env.funcs name
      {
        params = List.map (fun (vt, _) -> typ_of_source_type vt) params;
        ret = ret_t;
        is_variadic = false;
      };
    (ret_t, params)

(* typecheck a function declaration *)
and typecheck_func_decl env pos ret_type name params is_extern is_variadic =
  let ret_t, params =
    register_func_sig env pos ret_type name params is_variadic
  in
  FuncDecl
    {
      pos;
      ret_type = source_type_of_typ ret_t;
      name;
      params;
      is_extern;
      is_variadic;
    }

(* typecheck a function definition *)
and typecheck_func_def env pos ret_type name params body =
  let ret_t, params = register_func_sig env pos ret_type name params false in
  let fn_env =
    {
      vars = [ Hashtbl.create 8 ];
      funcs = env.funcs;
      typedefs = [ Hashtbl.create 8 ] @ env.typedefs;
      structs = env.structs;
      return_typ = Some ret_t;
      in_loop = false;
    }
  in
  (* then we add the variables to the env *)
  List.iter
    (fun (vt, pname) -> define_var fn_env pname (typ_of_source_type vt))
    params;
  let body = List.map (typecheck_stmt fn_env) body in
  if ret_t <> Void && name <> "main" && stmts_can_fall_through body then
    raise (Type_error (pos, MissingReturn name));
  FuncDef
    {
      pos;
      ret_type = source_type_of_typ ret_t;
      name;
      params;
      is_variadic = false;
      body;
    }

and typecheck_if env pos cond then_body else_body =
  let cond = typecheck_expr env cond in
  let cond = coerce_cond pos cond in
  (* recursively typecheck then_body *)
  let then_body = typecheck_stmt env then_body in
  (* recursively typecheck else_body is Some *)
  let else_body = Option.map (typecheck_stmt env) else_body in
  If { pos; cond; then_body; else_body }

and typecheck_while env pos cond body =
  let cond = typecheck_expr env cond in
  let cond = coerce_cond pos cond in
  let body = typecheck_stmt { env with in_loop = true } body in
  WhileLoop { pos; cond; body }

and typecheck_for env pos init cond incr body =
  let env = push_scope { env with in_loop = true } in
  let init = typecheck_stmt env init in
  let cond = Option.map (typecheck_expr env) cond in
  let incr = Option.map (typecheck_expr env) incr in
  let body = typecheck_stmt env body in
  begin match cond with
  | None -> ForLoop { pos; init; cond = None; incr; body }
  | Some cond ->
      let cond = coerce_cond pos cond in
      (* incr can be anything, we don't need to check its type *)
      ForLoop { pos; init; cond = Some cond; incr; body }
  end

and typecheck_do_while env pos body cond =
  let body = typecheck_stmt { env with in_loop = true } body in
  let cond = typecheck_expr env cond in
  let cond = coerce_cond pos cond in
  DoWhileLoop { pos; body; cond }
