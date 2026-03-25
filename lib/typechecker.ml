open Ast

type type_error =
  | UnknownType of string
  | UnboundVariable of string
  | UnboundFunction of string
  | MissingReturn of string
  | ArityMismatch of string * int * int (* name, expected, got *)
  | BinaryTypeMismatch of op * typ * typ
  | UnaryTypeMismatch of uop * typ
  | TypeMismatch of typ * typ (* expected, got *)
  | CondNotBool of typ
  | ReturnOutsideFunction
  | BreakOutsideLoop
  | ContinueOutsideLoop
  | NotLvalue
  | IncDecTypeMismatch of [ `Pre | `Post ] * [ `Inc | `Dec ] * typ
  | InvalidCast of typ * typ

exception Type_error of pos * type_error

let rec string_of_typ = function
  | Bool -> "bool"
  | Void -> "void"
  | Char -> "char"
  | SChar -> "signed char"
  | UChar -> "unsigned char"
  | Short -> "short"
  | UShort -> "unsigned short"
  | Int -> "int"
  | UInt -> "unsigned int"
  | Long -> "long"
  | ULong -> "unsigned long"
  | LongLong -> "long long"
  | ULongLong -> "unsigned long long"
  | Ptr t -> string_of_typ t ^ "*"
  | Array (t, sz) -> string_of_typ t ^ "[" ^ string_of_int sz ^ "]"

(** true for any integer type, including Bool, excluding Void *)
let is_integer_type = function
  | Bool
  | Char
  | SChar
  | UChar
  | Short
  | UShort
  | Int
  | UInt
  | Long
  | ULong
  | LongLong
  | ULongLong -> true
  | Ptr _ -> false
  | Array (_, _) -> false
  | Void -> false

let is_pointer_type = function
  | Ptr _ -> true
  | _ -> false

(** is a pointer, integer, float, not array, struct, union *)
let is_scalar_type = function
  | Void -> false
  | t -> is_integer_type t || is_pointer_type t

(** width in bits of an integer type *)
let integer_width = function
  | Bool -> 1
  | Char | SChar | UChar -> 8
  | Short | UShort -> 16
  | Int | UInt -> 32
  | Long | ULong | LongLong | ULongLong -> 64
  | Ptr _ -> 64
  | Array (_, _) | Void -> 0

(** rank of integers, in order of priority when casting implicitly *)
let integer_rank = function
  | Bool -> 0
  | Char | SChar | UChar -> 1
  | Short | UShort -> 2
  | Int | UInt -> 3
  | Long | ULong -> 4
  | LongLong | ULongLong -> 5
  | Ptr _ | Array (_, _) | Void -> assert false

(** true for signed integer types *)
let is_signed_type = function
  | Char | SChar | Short | Int | Long | LongLong -> true
  | UChar | UShort | UInt | ULong | ULongLong | Bool | Ptr _
  | Array (_, _)
  | Void -> false

(** gets the unsigned version of a signed type *)
let unsigned_counterpart = function
  | Char -> UChar
  | SChar -> UChar
  | Short -> UShort
  | Int -> UInt
  | Long -> ULong
  | LongLong -> ULongLong
  (* these types don't change *)
  | (UChar | UShort | UInt | ULong | ULongLong) as t -> t
  (* these types don't have an unsigned counterpart *)
  | Bool | Ptr _ | Array (_, _) | Void -> invalid_arg "unsigned counterpart"

let string_of_type_error = function
  | UnknownType name -> Printf.sprintf "unknown type '%s'" name
  | UnboundVariable x -> Printf.sprintf "unbound variable '%s'" x
  | UnboundFunction f -> Printf.sprintf "unbound function '%s'" f
  | MissingReturn f ->
      Printf.sprintf "control reaches end of non-void function '%s'" f
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
  | BreakOutsideLoop -> "break statement outside of a loop"
  | ContinueOutsideLoop -> "continue statement outside of a loop"
  | NotLvalue -> "expression is not an lvalue"
  | IncDecTypeMismatch (fix, dir, t) ->
      let fix_s =
        match fix with
        | `Pre -> "prefix"
        | `Post -> "postfix"
      in
      let dir_s =
        match dir with
        | `Inc -> "++"
        | `Dec -> "--"
      in
      Printf.sprintf "operator '%s %s': invalid operand type '%s'" fix_s dir_s
        (string_of_typ t)
  | InvalidCast (from_t, to_t) ->
      Printf.sprintf "cannot cast from '%s' to '%s'" (string_of_typ from_t)
        (string_of_typ to_t)

type func_sig = {
  params : typ list;
  ret : typ;
}

type env = {
  (* head is innermost scope. push when entering a new block, discard on exit *)
  vars : (string, typ) Hashtbl.t list;
  funcs : (string, func_sig) Hashtbl.t;
  return_typ : typ option;
  in_loop : bool;
}

let lookup_var env (x : string) =
  List.find_map (fun scope -> Hashtbl.find_opt scope x) env.vars

(* let rec lookup_var2 (env : env) (e : 'a expr) : typ option = match e with |
   VarRef (_, name) -> lookup_var env name | UnaryOp (_, Deref, inner) ->
   lookup_var2 env inner | _ -> failwith "undefined variable" *)

let define_var env name t =
  match env.vars with
  | scope :: _ -> Hashtbl.replace scope name t
  | [] -> failwith "empty scope stack"

let push_scope env = { env with vars = Hashtbl.create 8 :: env.vars }

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

(* converts a var_type to a typ, raising a user-facing error for unknown
   user-defined type names (VNamed). used during typechecking before VNamed
   types are guaranteed to be valid. *)
let resolve_var_type pos = function
  | VNamed name -> raise (Type_error (pos, UnknownType name))
  | vt -> Ast.typ_of_var_type vt

let expr_typ : checked expr -> typ = function
  | IntLiteral (ann, _)
  | BoolLiteral (ann, _)
  | CharLiteral (ann, _)
  | VarRef (ann, _)
  | BinaryOp (ann, _, _, _)
  | UnaryOp (ann, _, _)
  | Ternary (ann, _, _, _)
  | FuncCall (ann, _, _)
  | Assign (ann, _, _)
  | PreInc (ann, _)
  | PreDec (ann, _)
  | PostInc (ann, _)
  | PostDec (ann, _)
  | Cast (ann, _, _)
  | Subscript (ann, _, _)
  | ImplicitCast (ann, _, _) -> typ_of ann

let assert_lvalue (pos : pos) (e : checked expr) : unit =
  match e with
  | VarRef _ -> ()
  | UnaryOp (_, Deref, _) -> ()
  | _ -> raise (Type_error (pos, NotLvalue))

let check_binary pos op lt rt =
  let err () = raise (Type_error (pos, BinaryTypeMismatch (op, lt, rt))) in
  match op with
  | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | LShift | RShift ->
      if is_integer_type lt && lt = rt then lt else err ()
  | Less | Leq | Greater | Geq ->
      if is_integer_type lt && lt = rt then Bool else err ()
  | And | Or -> if lt = Bool && rt = Bool then Bool else err ()
  | Equal | Neq ->
      if (is_integer_type lt || is_pointer_type lt) && lt = rt then Bool
      else err ()

let check_unary pos op t =
  let err () = raise (Type_error (pos, UnaryTypeMismatch (op, t))) in
  match op with
  | Not -> if is_integer_type t then Bool else err ()
  | Neg | Compl -> if is_integer_type t then t else err ()
  | AddrOf | Deref -> assert false

(** insert an implicit cast node unless the type already matches *)
let implicit_cast pos to_t (e : checked expr) : checked expr =
  let from_t = expr_typ e in
  if from_t = to_t then
    (* same type, no cast needed *)
    e
  else
    (* different type, need a cast *)
    ImplicitCast (Checked (pos, to_t), to_t, e)

(** coerce conditional to bool *)
let coerce_cond pos (e : checked expr) : checked expr =
  let t = expr_typ e in
  if t = Bool then e
  else if is_scalar_type t then implicit_cast pos Bool e
  else raise (Type_error (pos, CondNotBool t))

(** two branches must have the same type *)
let check_ternary pos then_t else_t =
  if then_t <> else_t then
    raise (Type_error (pos, TypeMismatch (then_t, else_t)));
  then_t

(** if we can cast to a different scalar type *)
let can_explicit_cast from_t to_t =
  if from_t = to_t then true
  else
    match (from_t, to_t) with
    | Ptr _, Ptr _ | Ptr _, _ | _, Ptr _ ->
        (* if from or to or both is a pointer, ensure both are scalar *)
        is_scalar_type from_t && is_scalar_type to_t
    | _ ->
        (* otherwise, we need to ensure both are integer types *)
        is_integer_type from_t && is_integer_type to_t

(** if we can do an cast/conversion as if by assignment *)
let can_assign_cast from_t to_t =
  if from_t = to_t then true
  else
    match (from_t, to_t) with
    | Ptr _, Ptr Void | Ptr Void, Ptr _ ->
        (* void* cast is always allowed *)
        true
    | _ ->
        (* otherwise, check that both are integer types *)
        is_integer_type from_t && is_integer_type to_t

(** apply "conversion as if by assignment" *)
let cast_expr pos to_t (e : checked expr) : checked expr =
  let from_t = expr_typ e in
  if can_assign_cast from_t to_t then implicit_cast pos to_t e
  else raise (Type_error (pos, TypeMismatch (to_t, from_t)))

(** integer promotions used by unary and binary integer operators *)
let promote_integer pos (e : checked expr) : checked expr =
  let t = expr_typ e in
  (* it must be an integer type *)
  assert (is_integer_type t);
  if integer_rank t < integer_rank Int then
    (* if the rank is lower than int, cast it to int *)
    implicit_cast pos Int e
  else
    (* otherwise, leave it *)
    e

(** get the common type after the arithmetic conversions for integers. this
    should be an accurate implementation of the c conversion rules *)
let common_integer_type lt rt =
  if lt = rt then
    (* if they are the same, just return one of their types *)
    lt
  else if is_signed_type lt = is_signed_type rt then
    (* if they are both signed, then take the larger *)
    if integer_rank lt < integer_rank rt then rt else lt
  else
    (* if one is signed, the other isn't *)
    let unsigned_t, signed_t =
      if is_signed_type lt then (rt, lt) else (lt, rt)
    in
    if integer_rank unsigned_t >= integer_rank signed_t then
      (* if the unsigned one has a higher rank, use it *)
      unsigned_t
    else if integer_width signed_t > integer_width unsigned_t then
      (* if the signed has a larger width, use it *)
      signed_t
    else
      (* otherwise, use the the other type, but as unsigned *)
      unsigned_counterpart signed_t

let rec typecheck_expr (env : env) (expr : parsed expr) : checked expr =
  match expr with
  | IntLiteral (ann, n) -> typecheck_int_lit ann n
  | BoolLiteral (ann, b) -> typecheck_bool_lit ann b
  | CharLiteral (ann, c) -> typecheck_char_lit ann c
  | BinaryOp (ann, op, lhs, rhs) -> typecheck_binary_op env ann op lhs rhs
  | VarRef (ann, x) -> typecheck_var_ref env ann x
  | UnaryOp (ann, op, e) -> typecheck_unary_op env ann op e
  | Ternary (ann, cond, t, e) -> typecheck_ternary_op env ann cond t e
  | FuncCall (ann, f, args) -> typecheck_func_call env ann f args
  | Assign (ann, x, e) -> typecheck_assign env ann x e
  | PreInc (ann, e) ->
      typecheck_incdec env ann `Pre `Inc e (fun a x -> PreInc (a, x))
  | PreDec (ann, e) ->
      typecheck_incdec env ann `Pre `Dec e (fun a x -> PreDec (a, x))
  | PostInc (ann, e) ->
      typecheck_incdec env ann `Post `Inc e (fun a x -> PostInc (a, x))
  | PostDec (ann, e) ->
      typecheck_incdec env ann `Post `Dec e (fun a x -> PostDec (a, x))
  | Subscript (_ann, _a, _i) -> failwith "todo"
  | Cast (ann, var_type, e) ->
      let pos = pos_of ann in
      let to_t = resolve_var_type pos var_type in
      let e = typecheck_expr env e in
      let from_t = expr_typ e in
      if not (can_explicit_cast from_t to_t) then
        raise (Type_error (pos, InvalidCast (from_t, to_t)));
      Cast (Checked (pos, to_t), var_type, e)
  | ImplicitCast (_ann, _ty, _e) -> assert false

and typecheck_int_lit (ann : parsed ann) (n : int) : checked expr =
  (* decimal literals outside of the 32 bit range are turned into longs *)
  (* todo: might overflow bc we can represent 64 bit integers but integers in
  ocaml only have 63 bits *)
  let t = if n >= -2147483648 && n <= 2147483647 then Int else Long in
  IntLiteral (Checked (pos_of ann, t), n)

and typecheck_bool_lit (ann : parsed ann) (b : bool) : checked expr =
  BoolLiteral (Checked (pos_of ann, Bool), b)

and typecheck_char_lit (ann : parsed ann) (c : int) : checked expr =
  CharLiteral (Checked (pos_of ann, Char), c)

and typecheck_binary_op env ann op lhs rhs =
  let pos = pos_of ann in
  let lhs = typecheck_expr env lhs in
  let rhs = typecheck_expr env rhs in
  let err () =
    raise
      (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)))
  in
  match op with
  | And | Or ->
      (* in C, && and || accept any scalar type; coerce operands to bool *)
      if not (is_scalar_type (expr_typ lhs) && is_scalar_type (expr_typ rhs))
      then err ();
      let lhs = implicit_cast pos Bool lhs in
      let rhs = implicit_cast pos Bool rhs in
      BinaryOp (Checked (pos, Bool), op, lhs, rhs)
  | (Less | Leq | Greater | Geq | Equal | Neq)
    when is_pointer_type (expr_typ lhs) && is_pointer_type (expr_typ rhs) ->
      (* comparisons between pointers *)
      if expr_typ lhs = expr_typ rhs then
        BinaryOp (Checked (pos, Bool), op, lhs, rhs)
      else
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)))
  | Add
    when (is_pointer_type (expr_typ lhs) && is_integer_type (expr_typ rhs))
         || (is_integer_type (expr_typ lhs) && is_pointer_type (expr_typ rhs))
    ->
      (* ptr + int or int + ptr — void* arithmetic is rejected *)
      if expr_typ lhs = Ptr Void || expr_typ rhs = Ptr Void then
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)));
      let t =
        if is_pointer_type (expr_typ lhs) then expr_typ lhs else expr_typ rhs
      in
      BinaryOp (Checked (pos, t), op, lhs, rhs)
  | Sub when is_pointer_type (expr_typ lhs) && is_integer_type (expr_typ rhs) ->
      (* ptr - int — void* arithmetic is rejected *)
      if expr_typ lhs = Ptr Void then
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)));
      BinaryOp (Checked (pos, expr_typ lhs), op, lhs, rhs)
  | Sub when is_pointer_type (expr_typ lhs) && is_pointer_type (expr_typ rhs) ->
      (* ptr - ptr: types must match; void* - void* is rejected *)
      if
        expr_typ lhs = Ptr Void
        || expr_typ rhs = Ptr Void
        || expr_typ lhs <> expr_typ rhs
      then
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)));
      BinaryOp (Checked (pos, Long), op, lhs, rhs)
  | LShift | RShift ->
      if not (is_integer_type (expr_typ lhs) && is_integer_type (expr_typ rhs))
      then err ();
      let lhs = promote_integer pos lhs in
      let rhs = promote_integer pos rhs in
      BinaryOp (Checked (pos, expr_typ lhs), op, lhs, rhs)
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | Less
  | Leq
  | Greater
  | Geq
  | Equal
  | Neq ->
      if not (is_integer_type (expr_typ lhs) && is_integer_type (expr_typ rhs))
      then err ();
      let lhs = promote_integer pos lhs in
      let rhs = promote_integer pos rhs in
      let common_t = common_integer_type (expr_typ lhs) (expr_typ rhs) in
      let lhs = implicit_cast pos common_t lhs in
      let rhs = implicit_cast pos common_t rhs in
      let t =
        match op with
        | Less | Leq | Greater | Geq | Equal | Neq -> Bool
        | _ -> common_t
      in
      BinaryOp (Checked (pos, t), op, lhs, rhs)

and typecheck_var_ref env ann x =
  let pos = pos_of ann in
  (* error if the variable doesn't exist *)
  let t =
    match lookup_var env x with
    | Some t -> t
    | None -> raise (Type_error (pos, UnboundVariable x))
  in
  VarRef (Checked (pos, t), x)

and typecheck_unary_op env ann op e =
  let pos = pos_of ann in
  let e = typecheck_expr env e in
  let t = expr_typ e in
  match op with
  | AddrOf ->
      (* ensure is a lvalue *)
      assert_lvalue pos e;
      UnaryOp (Checked (pos, Ptr t), op, e)
  | Deref -> begin
      (* ensure only deref a pointer *)
      match t with
      | Ptr tt when tt <> Void ->
          (* ensure the pointer is not void *)
          UnaryOp (Checked (pos, tt), op, e)
      | t -> raise (Type_error (pos, UnaryTypeMismatch (op, t)))
    end
  | Neg | Compl ->
      if not (is_integer_type t) then
        raise (Type_error (pos, UnaryTypeMismatch (op, t)));
      let e = promote_integer pos e in
      UnaryOp (Checked (pos, expr_typ e), op, e)
  | Not ->
      let t = check_unary pos op t in
      UnaryOp (Checked (pos, t), op, e)

and typecheck_ternary_op env ann cond then_e else_e =
  let pos = pos_of ann in
  let cond = typecheck_expr env cond in
  let cond = coerce_cond pos cond in
  let then_e = typecheck_expr env then_e in
  let else_e = typecheck_expr env else_e in
  let then_t = expr_typ then_e in
  let else_t = expr_typ else_e in
  (* for integer types, apply standard arithmetic conversions *)
  if is_integer_type then_t && is_integer_type else_t then
    let then_e = promote_integer pos then_e in
    let else_e = promote_integer pos else_e in
    let common_t = common_integer_type (expr_typ then_e) (expr_typ else_e) in
    let then_e = implicit_cast pos common_t then_e in
    let else_e = implicit_cast pos common_t else_e in
    Ternary (Checked (pos, common_t), cond, then_e, else_e)
  else
    (* for non-integer types, require exact match *)
    let t = check_ternary pos then_t else_t in
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
        let arg = cast_expr pos param_t arg in
        let at = expr_typ arg in
        if at <> param_t then
          raise (Type_error (pos, TypeMismatch (param_t, at)));
        arg)
      sig_.params args
  in
  FuncCall (Checked (pos, sig_.ret), f, args)

(** typecheck an increment/decrement operation. [make] is a function that makes
    the checked expr node, as there are four very similar cases *)
and typecheck_incdec env ann fix dir operand make =
  let pos = pos_of ann in
  let e = typecheck_expr env operand in
  assert_lvalue pos e;
  let t = expr_typ e in
  if (not (is_integer_type t)) && not (is_pointer_type t) then
    raise (Type_error (pos, IncDecTypeMismatch (fix, dir, t)));
  make (Checked (pos, t)) e

and typecheck_stmt (env : env) (stmt : parsed stmt) : checked stmt =
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
  | VarDef { pos; var_type; name; init } ->
      typecheck_var_def env pos var_type name init
  | FuncDef { pos; ret_type; name; params; body } ->
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

and typecheck_var_def env pos var_type name init =
  let var_t = resolve_var_type pos var_type in
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
  VarDef { pos; var_type; name; init }

and typecheck_func_def env pos ret_type name params body =
  let ret_t = resolve_var_type pos ret_type in
  (* add function to env first *)
  Hashtbl.replace env.funcs name
    {
      params = List.map (fun (vt, _) -> resolve_var_type pos vt) params;
      ret = ret_t;
    };
  let fn_env =
    {
      vars = [ Hashtbl.create 8 ];
      funcs = env.funcs;
      return_typ = Some ret_t;
      in_loop = false;
    }
  in
  (* then we add the variables to the env *)
  List.iter
    (fun (vt, pname) -> define_var fn_env pname (resolve_var_type pos vt))
    params;
  let body = List.map (typecheck_stmt fn_env) body in
  if ret_t <> Void && name <> "main" && stmts_can_fall_through body then
    raise (Type_error (pos, MissingReturn name));
  FuncDef { pos; ret_type; name; params; body }

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

and typecheck_assign env ann lhs rhs =
  let pos = pos_of ann in
  let lhs = typecheck_expr env lhs in
  (* lhs must be a lvalue *)
  assert_lvalue pos lhs;
  let lhs_t = expr_typ lhs in
  let rhs = typecheck_expr env rhs in
  let rhs = cast_expr pos lhs_t rhs in
  let rhs_t = expr_typ rhs in
  if rhs_t <> lhs_t then raise (Type_error (pos, TypeMismatch (lhs_t, rhs_t)));
  Assign (Checked (pos, lhs_t), lhs, rhs)

let typecheck_program (stmts : parsed stmt list) : checked stmt list =
  let funcs = Hashtbl.create 8 in
  (* "stdlib" functions *)
  Hashtbl.replace funcs "printint" { params = [ Int ]; ret = Void };
  Hashtbl.replace funcs "printbool" { params = [ Bool ]; ret = Void };
  let env =
    { vars = [ Hashtbl.create 8 ]; funcs; return_typ = None; in_loop = false }
  in
  List.map (typecheck_stmt env) stmts
