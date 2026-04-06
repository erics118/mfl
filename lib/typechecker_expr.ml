open Ast
open Typechecker_types
open Typechecker_error
open Typechecker_env

(* converts a source_type to a typ, raising a user-facing error for unknown
   user-defined type names (VNamed). used during typechecking before VNamed
   types are guaranteed to be valid. *)
let resolve_source_type env pos source_type =
  let rec go seen = function
    | VNamed name -> begin
        if List.mem name seen then raise (Type_error (pos, UnknownType name));
        match lookup_typedef env name with
        | Some vt -> go (name :: seen) vt
        | None -> raise (Type_error (pos, UnknownType name))
      end
    | VStruct tag ->
        (* don't require struct to be defined here; pointers to incomplete
           structs are allowed. definition is checked at var decl and access *)
        Struct tag
    | VPtr t -> Ptr (go seen t)
    | VArray (t, sz) -> Array (go seen t, sz)
    | vt -> Ast.typ_of_source_type vt
  in
  go [] source_type

let assert_lvalue (pos : pos) (e : checked expr) : unit =
  match e with
  | VarRef _ -> ()
  | UnaryOp (_, Deref, _) -> ()
  | Subscript _ -> ()
  | MemberAccess _ -> ()
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
  | Not -> if is_scalar_type t then Bool else err ()
  | Neg -> if is_arithmetic_type t then t else err ()
  | Compl -> if is_integer_type t then t else err ()
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
  if from_t = to_t then (* allow same types to cast (redundant) *)
    true
  else if is_arithmetic_type from_t && is_arithmetic_type to_t then
    (* arithmetic types can cast from each other *)
    true
  else if is_pointer_type from_t && is_pointer_type to_t then
    (* pointers can cast to any type of pointer *)
    true
  else if
    (is_integer_type from_t && is_pointer_type to_t)
    || (is_pointer_type from_t && is_integer_type to_t)
  then
    (* pointer and int can cast freely *)
    true
  else
    (* no other types are allowed *)
    false

(** if we can do an cast/conversion as if by assignment *)
let can_assign_cast from_t to_t =
  if from_t = to_t then (* allow same types to cast (redundant) *)
    true
  else if
    (is_pointer_type from_t && to_t = Ptr Void)
    || (from_t = Ptr Void && is_pointer_type to_t)
  then
    (* we can explicitly cast to/from void* *)
    true
  else
    match (from_t, to_t) with
    | Array (t, _), Ptr t2 ->
        (* array decays to pointer to its element type *)
        t = t2
    | _ ->
        (* otherwise, only integer-to-integer assignment conversions are
           currently allowed *)
        is_arithmetic_type from_t && is_arithmetic_type to_t

(** the integer constant 0 can be implicitly converted to any pointer type *)
let is_null_ptr_constant = function
  | IntLiteral (_, 0) -> true
  | _ -> false

(** apply "conversion as if by assignment" *)
let cast_expr pos to_t (e : checked expr) : checked expr =
  let from_t = expr_typ e in
  if can_assign_cast from_t to_t then
    (* assign cast *)
    implicit_cast pos to_t e
  else if is_pointer_type to_t && is_null_ptr_constant e then
    (* 0, ie NULL, can convert to any pointer type *)
    implicit_cast pos to_t e
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

(** get the common type for arithmetic operands. integers use the usual integer
    conversions; float and double dominate integer types. *)
let common_arithmetic_type lt rt =
  if is_float_type lt || is_float_type rt then
    (* if either is a double, then result type is double *)
    if lt = Double || rt = Double then Double else Float
  else
    (* otherwise, ints *)
    common_integer_type lt rt

let rec typecheck_expr (env : env) (expr : parsed expr) : checked expr =
  match expr with
  | IntLiteral (ann, n) -> typecheck_int_lit ann n
  | FloatLiteral (ann, f) -> FloatLiteral (Checked (pos_of ann, Float), f)
  | DoubleLiteral (ann, f) -> DoubleLiteral (Checked (pos_of ann, Double), f)
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
  | Subscript (ann, a, i) -> typecheck_subscript env ann a i
  | Cast (ann, source_type, e) ->
      let pos = pos_of ann in
      let to_t = resolve_source_type env pos source_type in
      let e = typecheck_expr env e in
      let from_t = expr_typ e in
      if not (can_explicit_cast from_t to_t) then
        raise (Type_error (pos, InvalidCast (from_t, to_t)));
      Cast (Checked (pos, to_t), source_type_of_typ to_t, e)
  | MemberAccess (ann, lhs, field) ->
      let pos = pos_of ann in
      let lhs = typecheck_expr env lhs in
      let tag =
        match expr_typ lhs with
        | Struct tag -> tag
        | t -> raise (Type_error (pos, NotAStruct t))
      in
      let fields =
        match lookup_struct env tag with
        | Some fs -> fs
        | None -> raise (Type_error (pos, UnknownType ("struct " ^ tag)))
      in
      let field_t =
        match List.assoc_opt field fields with
        | Some t -> t
        | None -> raise (Type_error (pos, NoSuchField (tag, field)))
      in
      MemberAccess (Checked (pos, field_t), lhs, field)
  | ImplicitCast (_ann, _ty, _e) -> assert false
  | SizeofExpr (ann, e) ->
      let pos = pos_of ann in
      let e = typecheck_expr env e in
      (* sizeof returns a long *)
      SizeofExpr (Checked (pos, Long), e)
  | SizeofType (ann, t) ->
      let pos = pos_of ann in
      (* we just need to ensure it is a valid type *)
      let t = resolve_source_type env pos t in
      SizeofType (Checked (pos, Long), source_type_of_typ t)

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
  let lt = expr_typ lhs in
  let rt = expr_typ rhs in
  let err () =
    raise
      (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)))
  in
  match op with
  (* &&, || for scalar *)
  | And | Or ->
      (* scalar operands coerce to bool *)
      if not (is_scalar_type lt && is_scalar_type rt) then err ();
      let lhs' = implicit_cast pos Bool lhs in
      let rhs' = implicit_cast pos Bool rhs in
      BinaryOp (Checked (pos, Bool), op, lhs', rhs')
  (* =, != for ptr *)
  | (Equal | Neq) when is_pointer_type lt && is_null_ptr_constant rhs ->
      (* ptr == 0 or ptr != 0: cast 0 to the pointer type *)
      BinaryOp (Checked (pos, Bool), op, lhs, implicit_cast pos lt rhs)
  (* =, != ptr *)
  | (Equal | Neq) when is_null_ptr_constant lhs && is_pointer_type rt ->
      (* 0 == ptr or 0 != ptr: cast 0 to pointer type *)
      BinaryOp (Checked (pos, Bool), op, implicit_cast pos rt lhs, rhs)
  (* cmp for ptr *)
  | (Less | Leq | Greater | Geq | Equal | Neq)
    when is_pointer_type lt && is_pointer_type rt ->
      (* comparisons between pointers *)
      if expr_typ lhs = expr_typ rhs then
        BinaryOp (Checked (pos, Bool), op, lhs, rhs)
      else
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)))
  (* ptr + int, int + ptr *)
  | Add
    when (is_pointer_type lt && is_integer_type (expr_typ rhs))
         || (is_integer_type lt && is_pointer_type (expr_typ rhs)) ->
      (* ptr + int or int + ptr, reject void* *)
      if lt = Ptr Void || rt = Ptr Void then
        raise
          (Type_error (pos, BinaryTypeMismatch (op, expr_typ lhs, expr_typ rhs)));
      let t = if is_pointer_type lt then expr_typ lhs else expr_typ rhs in
      BinaryOp (Checked (pos, t), op, lhs, rhs)
  | Sub when is_pointer_type lt && is_integer_type rt ->
      (* ptr - int, reject void* *)
      if lt = Ptr Void then
        raise (Type_error (pos, BinaryTypeMismatch (op, lt, rt)));
      BinaryOp (Checked (pos, lt), op, lhs, rhs)
  (* ptr - ptr *)
  | Sub when is_pointer_type lt && is_pointer_type rt ->
      (* ptr - ptr: types must match; void* - void* is rejected *)
      if lt = Ptr Void || rt = Ptr Void || lt <> rt then
        raise (Type_error (pos, BinaryTypeMismatch (op, lt, rt)));
      BinaryOp (Checked (pos, Long), op, lhs, rhs)
  (* bit shift between int and int *)
  | LShift | RShift ->
      if not (is_integer_type lt && is_integer_type rt) then err ();
      let lhs' = promote_integer pos lhs in
      let rhs' = promote_integer pos rhs in
      BinaryOp (Checked (pos, expr_typ lhs'), op, lhs', rhs')
  (* arithmetic for float *)
  | (Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor)
    when not (is_integer_type lt && is_integer_type rt) -> err ()
  (* cmp for floats *)
  | (Less | Leq | Greater | Geq | Equal | Neq)
    when is_arithmetic_type lt && is_arithmetic_type rt ->
      let lhs' = if is_integer_type lt then promote_integer pos lhs else lhs in
      let rhs' = if is_integer_type rt then promote_integer pos rhs else rhs in
      let common_t = common_arithmetic_type (expr_typ lhs) (expr_typ rhs) in
      let lhs' = implicit_cast pos common_t lhs' in
      let rhs' = implicit_cast pos common_t rhs' in
      BinaryOp (Checked (pos, Bool), op, lhs', rhs')
  (* arithmetic for int *)
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
      if not (is_integer_type lt && is_integer_type rt) then err ();
      let lhs' = promote_integer pos lhs in
      let rhs' = promote_integer pos rhs in
      let common_t = common_integer_type (expr_typ lhs') (expr_typ rhs') in
      let lhs' = implicit_cast pos common_t lhs' in
      let rhs' = implicit_cast pos common_t rhs' in
      let t =
        match op with
        | Less | Leq | Greater | Geq | Equal | Neq -> Bool
        | _ -> common_t
      in
      BinaryOp (Checked (pos, t), op, lhs', rhs')

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
  | Neg ->
      if not (is_arithmetic_type t) then
        raise (Type_error (pos, UnaryTypeMismatch (op, t)));
      let e = if is_integer_type t then promote_integer pos e else e in
      UnaryOp (Checked (pos, expr_typ e), op, e)
  | Compl ->
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

and typecheck_subscript env ann a i =
  let pos = pos_of ann in
  let a = typecheck_expr env a in
  let i = typecheck_expr env i in
  (* array/pointer being subscripted must decay to a pointer *)
  let elem_t =
    match expr_typ a with
    | Array (t, _) -> t
    | Ptr t when t <> Void -> t
    | t -> raise (Type_error (pos, UnaryTypeMismatch (Deref, t)))
  in
  (* index must be an integer *)
  if not (is_integer_type (expr_typ i)) then
    raise (Type_error (pos, UnaryTypeMismatch (Deref, expr_typ i)));
  (* promote index to an int *)
  let i = promote_integer pos i in
  Subscript (Checked (pos, elem_t), a, i)
