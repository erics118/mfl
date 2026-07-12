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

let rec typecheck_stmts env = function
  | [] -> (env, [])
  | stmt :: rest ->
      let env, checked_stmt = typecheck_stmt_with_env env stmt in
      let env, checked_rest = typecheck_stmts env rest in
      (env, checked_stmt :: checked_rest)

and typecheck_stmt_with_env (env : env) (stmt : parsed stmt) :
    env * checked stmt =
  match stmt with
  | EmptyStmt pos ->
      (* trivial *)
      (env, EmptyStmt pos)
  | BreakStmt pos ->
      if not env.in_loop then type_error pos BreakOutsideLoop;
      (env, BreakStmt pos)
  | ContinueStmt pos ->
      if not env.in_loop then type_error pos ContinueOutsideLoop;
      (env, ContinueStmt pos)
  | ExprStmt (pos, e) ->
      (* we can just check the inner expr *)
      (env, ExprStmt (pos, typecheck_expr env e))
  | ReturnStmt (pos, e) -> typecheck_return env pos e
  | CompoundStmt (pos, stmts) ->
      (* create a new environment *)
      let scoped_env, stmts = typecheck_stmts (push_scope env) stmts in
      let env = with_globals env scoped_env.globals in
      (env, CompoundStmt (pos, stmts))
  | Typedef { pos; struct_def; existing_type; alias } ->
      (* if there's an inline struct def, register it first *)
      let env, struct_def =
        match struct_def with
        | None -> (env, None)
        | Some (tag, fields) ->
            let resolved =
              List.map
                (fun (vt, fname) -> (fname, resolve_source_type env pos vt))
                fields
            in
            let env = define_struct env tag resolved in
            ( env,
              Some
                ( tag,
                  List.map
                    (fun (fname, typ) -> (source_type_of_typ typ, fname))
                    resolved ) )
      in
      let existing_type =
        source_type_of_typ (resolve_source_type env pos existing_type)
      in
      let env = define_typedef env alias existing_type in
      (env, Typedef { pos; struct_def; existing_type; alias })
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
    | None -> type_error pos ReturnOutsideFunction
    | Some t -> t
  in
  match e with
  | None ->
      (* if the return value is also void, good *)
      if ret <> Void then type_error pos (TypeMismatch (ret, Void));
      (env, ReturnStmt (pos, None))
  | Some e ->
      (* ensure that return value is equal to ret, the correct return type *)
      let e = typecheck_expr env e in
      let e = cast_expr_at pos ret e in
      let t = expr_typ e in
      if t <> ret then type_error pos (TypeMismatch (ret, t));
      (env, ReturnStmt (pos, Some e))

and typecheck_var_def env pos source_type name init =
  let var_t = resolve_source_type env pos source_type in
  (* if init exists, we check its type to ensure it is valid *)
  let init =
    match init with
    | None -> None
    | Some init -> begin
        let init = typecheck_expr env init in
        let init = cast_expr_at pos var_t init in
        let init_t = expr_typ init in
        (* ensure init has the right type *)
        if var_t <> init_t then type_error pos (TypeMismatch (var_t, init_t));
        Some init
      end
  in
  let env = define_var env name var_t in
  (env, VarDef { pos; source_type = source_type_of_typ var_t; name; init })

and typecheck_struct_def env pos tag fields var_name =
  (* resolve each field type and register the struct *)
  let resolved =
    List.map (fun (vt, fname) -> (fname, resolve_source_type env pos vt)) fields
  in
  let env = define_struct env tag resolved in
  (* if var_name is given, also define a variable of this struct type *)
  let env =
    match var_name with
    | None -> env
    | Some name -> define_var env name (Struct tag)
  in
  let fields =
    List.map (fun (fname, typ) -> (source_type_of_typ typ, fname)) resolved
  in
  (env, StructDef { pos; tag; fields; var_name })

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
and register_func_sig env pos ret_type name params ~is_variadic =
  let ret_t = resolve_source_type env pos ret_type in
  let params = normalize_params env pos params in
  let env =
    define_func env name
      {
        params = List.map (fun (vt, _) -> typ_of_source_type vt) params;
        ret = ret_t;
        is_variadic;
      }
  in
  (env, ret_t, params)

(* typecheck a function declaration *)
and typecheck_func_decl env pos ret_type name params is_extern is_variadic =
  let env, ret_t, params =
    register_func_sig env pos ret_type name params ~is_variadic
  in
  ( env,
    FuncDecl
      {
        pos;
        ret_type = source_type_of_typ ret_t;
        name;
        params;
        is_extern;
        is_variadic;
      } )

(* typecheck a function definition *)
and typecheck_func_def env pos ret_type name params body =
  let env, ret_t, params =
    register_func_sig env pos ret_type name params ~is_variadic:false
  in
  let fn_env =
    {
      vars = [ StringMap.empty ];
      globals = env.globals;
      typedefs = StringMap.empty :: env.typedefs;
      return_typ = Some ret_t;
      in_loop = false;
    }
  in
  (* then we add the variables to the env *)
  let fn_env =
    List.fold_left
      (fun env (vt, pname) -> define_var env pname (typ_of_source_type vt))
      fn_env params
  in
  let fn_env, body = typecheck_stmts fn_env body in
  if ret_t <> Void && name <> "main" && stmts_can_fall_through body then
    type_error pos (MissingReturn name);
  let env = with_globals env fn_env.globals in
  ( env,
    FuncDef
      {
        pos;
        ret_type = source_type_of_typ ret_t;
        name;
        params;
        is_variadic = false;
        body;
      } )

and typecheck_if env pos cond then_body else_body =
  let cond = typecheck_expr env cond in
  let cond = coerce_cond_at pos cond in
  (* recursively typecheck then_body *)
  let env, then_body = typecheck_stmt_with_env env then_body in
  (* recursively typecheck else_body is Some *)
  let env, else_body =
    match else_body with
    | None -> (env, None)
    | Some else_body ->
        let env, else_body = typecheck_stmt_with_env env else_body in
        (env, Some else_body)
  in
  (env, If { pos; cond; then_body; else_body })

and typecheck_while env pos cond body =
  let cond = typecheck_expr env cond in
  let cond = coerce_cond_at pos cond in
  let body_env, body =
    typecheck_stmt_with_env { env with in_loop = true } body
  in
  let env = with_globals env body_env.globals in
  (env, WhileLoop { pos; cond; body })

and typecheck_for env pos init cond incr body =
  let scoped_env = push_scope { env with in_loop = true } in
  let scoped_env, init = typecheck_stmt_with_env scoped_env init in
  let cond = Option.map (typecheck_expr scoped_env) cond in
  let incr = Option.map (typecheck_expr scoped_env) incr in
  let scoped_env, body = typecheck_stmt_with_env scoped_env body in
  let env = with_globals env scoped_env.globals in
  begin match cond with
  | None -> (env, ForLoop { pos; init; cond = None; incr; body })
  | Some cond ->
      let cond = coerce_cond_at pos cond in
      (* incr can be anything, we don't need to check its type *)
      (env, ForLoop { pos; init; cond = Some cond; incr; body })
  end

and typecheck_do_while env pos body cond =
  let body_env, body =
    typecheck_stmt_with_env { env with in_loop = true } body
  in
  let cond = typecheck_expr env cond in
  let cond = coerce_cond_at pos cond in
  let env = with_globals env body_env.globals in
  (env, DoWhileLoop { pos; body; cond })

let typecheck_stmt env stmt = snd (typecheck_stmt_with_env env stmt)
