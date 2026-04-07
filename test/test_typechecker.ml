open OUnit2
open Mfl
open Ast
open Typechecker

let dummy_pos : pos = { line = 0; col = 0 }
let p = Parsed dummy_pos

let make_tbl pairs =
  let tbl = Hashtbl.create 4 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) pairs;
  tbl

let default_env () =
  {
    vars = [ Hashtbl.create 4 ];
    funcs =
      make_tbl [ ("noop", { params = []; ret = Void; is_variadic = false }) ];
    typedefs = [ make_tbl [] ];
    structs = make_tbl [];
    return_typ = None;
    in_loop = false;
  }

let i n = IntLiteral (p, n, NoIntSuffix)
let iu n = IntLiteral (p, n, UnsignedSuffix)
let il n = IntLiteral (p, n, LongSuffix)
let iul n = IntLiteral (p, n, UnsignedLongSuffix)
let ill n = IntLiteral (p, n, LongLongSuffix)
let iull n = IntLiteral (p, n, UnsignedLongLongSuffix)
let f x = FloatLiteral (p, x)
let d x = DoubleLiteral (p, x)
let ld x = LongDoubleLiteral (p, x)
let b b = BoolLiteral (p, b)
let s bytes = StringLiteral (p, bytes)
let bi op l r = BinaryOp (p, op, l, r)
let un op e = UnaryOp (p, op, e)
let tern cond then_e else_e = Ternary (p, cond, then_e, else_e)

(* we aren't using mutable stuff here, so we shadow ! to mean variable reference
   and := to mean variable assignment, and we add $ to mean function
   call/application *)
let ( ! ) x = VarRef (p, x)
let ( := ) x e = Assign (p, !x, e)
let ( $ ) f args = FuncCall (p, f, args)
let cast ty e = Cast (p, ty, e)
let pre_inc e = PreInc (p, e)
let post_inc e = PostInc (p, e)
let pre_dec e = PreDec (p, e)
let post_dec e = PostDec (p, e)

(* noop function call, used to get a void type *)
let noop = "noop" $ []

(* statement constructors that fill in dummy_pos and common defaults *)
let empty = EmptyStmt dummy_pos
let ret e = ReturnStmt (dummy_pos, Some e)
let ret_void = ReturnStmt (dummy_pos, None)

let var_def ?(init = None) vt name =
  VarDef { pos = dummy_pos; source_type = vt; name; init }

let func_def ?(is_variadic = false) rt name params body =
  FuncDef { pos = dummy_pos; ret_type = rt; name; params; is_variadic; body }

let func_decl ?(is_variadic = false) ?(is_extern = false) rt name params =
  FuncDecl
    { pos = dummy_pos; ret_type = rt; name; params; is_variadic; is_extern }

let if_ ?(else_ = None) cond body =
  If { pos = dummy_pos; cond; then_body = body; else_body = else_ }

let while_ cond body = WhileLoop { pos = dummy_pos; cond; body }
let do_while_ body cond = DoWhileLoop { pos = dummy_pos; body; cond }

let for_ ?(cond = None) ?(incr = None) init body =
  ForLoop { pos = dummy_pos; init; cond; incr; body }

let env_with vars = { (default_env ()) with vars = [ make_tbl vars ] }

let env_with_funcs funcs =
  {
    (default_env ()) with
    funcs =
      make_tbl
        (funcs @ [ ("noop", { params = []; ret = Void; is_variadic = false }) ]);
  }

let env_with_all vars funcs =
  {
    (default_env ()) with
    vars = [ make_tbl vars ];
    funcs =
      make_tbl
        (funcs @ [ ("noop", { params = []; ret = Void; is_variadic = false }) ]);
  }

let all_integer_types =
  [
    Bool;
    Char;
    SChar;
    UChar;
    Short;
    UShort;
    Int;
    UInt;
    Long;
    ULong;
    LongLong;
    ULongLong;
  ]

let name_of_typ = function
  | Bool -> "b"
  | Char -> "c"
  | SChar -> "sc"
  | UChar -> "uc"
  | Short -> "s"
  | UShort -> "us"
  | Int -> "i"
  | UInt -> "u"
  | Long -> "l"
  | ULong -> "ul"
  | LongLong -> "ll"
  | ULongLong -> "ull"
  | Float -> "f"
  | Double -> "d"
  | LongDouble -> "ld"
  | Ptr _ -> "p"
  | Array (_, _) -> "arr"
  | Void -> "v"
  | Struct _ -> "s"

let all_integer_vars = List.map (fun t -> (name_of_typ t, t)) all_integer_types

let expected_promoted_type t =
  if integer_rank t < integer_rank Int then Int else t

let check_typ ?(env = default_env ()) expected e =
  match typecheck_expr env e with
  | result -> assert_equal ~printer:string_of_typ expected (expr_typ result)
  | exception Type_error (_, err) -> assert_failure (string_of_type_error err)

let check_err ?(env = default_env ()) expected_err e =
  match typecheck_expr env e with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let check_stmt_err ?(env = default_env ()) expected_err s =
  match typecheck_stmt env s with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let assert_implicit_cast_to expected = function
  | ImplicitCast (Checked (_, t), _, _) ->
      assert_equal ~printer:string_of_typ expected t
  | _ -> assert_failure "expected ImplicitCast"

let test_type_helpers _ =
  assert_equal "char" (string_of_typ Char);
  assert_equal "signed char" (string_of_typ SChar);
  assert_equal "unsigned char" (string_of_typ UChar);
  assert_equal "short" (string_of_typ Short);
  assert_equal "unsigned short" (string_of_typ UShort);
  assert_equal "unsigned int" (string_of_typ UInt);
  assert_equal "unsigned long" (string_of_typ ULong);
  assert_equal "long long" (string_of_typ LongLong);
  assert_equal "unsigned long long" (string_of_typ ULongLong);
  assert_equal "long double" (string_of_typ LongDouble);
  assert_equal "int*" (string_of_typ (Ptr Int));
  assert_equal "char[4]" (string_of_typ (Array (Char, 4)));
  assert_equal "struct Foo" (string_of_typ (Struct "__anon_Foo"));
  assert_bool "bool is an integer type" (is_integer_type Bool);
  assert_bool "int ptr is not an integer type" (not (is_integer_type (Ptr Int)));
  assert_bool "arr int[4] is not an integer type"
    (not (is_integer_type (Array (Int, 4))));
  assert_bool "int ptr is a pointer type" (is_pointer_type (Ptr Int));
  assert_bool "int is not a pointer type" (not (is_pointer_type Int));
  assert_bool "pointer is scalar" (is_scalar_type (Ptr Int));
  assert_bool "void is not scalar" (not (is_scalar_type Void));
  assert_equal ~printer:string_of_int 64 (integer_width (Ptr Int));
  assert_equal ~printer:string_of_int 0 (integer_width Void);
  assert_bool "char is signed" (is_signed_type Char);
  assert_bool "ulong is unsigned" (not (is_signed_type ULong));
  assert_equal ~printer:string_of_typ UInt (unsigned_counterpart Int);
  assert_equal ~printer:string_of_typ ULongLong (unsigned_counterpart ULongLong)

let test_type_error_strings _ =
  assert_equal "return statement outside of a function"
    (string_of_type_error ReturnOutsideFunction);
  assert_equal "break statement outside of a loop"
    (string_of_type_error BreakOutsideLoop);
  assert_equal "continue statement outside of a loop"
    (string_of_type_error ContinueOutsideLoop);
  assert_equal "expression is not an lvalue" (string_of_type_error NotLvalue);
  assert_equal "operator 'postfix --': invalid operand type 'int'"
    (string_of_type_error (IncDecTypeMismatch (`Post, `Dec, Int)));
  assert_equal "cannot cast from 'int' to 'void'"
    (string_of_type_error (InvalidCast (Int, Void)));
  assert_equal "struct 'Foo' has no field 'y'"
    (string_of_type_error (NoSuchField ("__anon_Foo", "y")))

let test_stmt_fallthrough_helpers _ =
  assert_bool "return does not fall through"
    (not (stmt_can_fall_through ret_void));
  assert_bool "empty stmt falls through" (stmt_can_fall_through empty);
  assert_bool "if without else falls through"
    (stmt_can_fall_through (if_ (b true) (ret (i 1))));
  assert_bool "both return branches do not fall through"
    (not
       (stmt_can_fall_through
          (if_ ~else_:(Some (ret (i 2))) (b true) (ret (i 1)))));
  assert_bool "stmt list stops at first non-fallthrough"
    (not (stmts_can_fall_through [ ret (i 1); empty ]))

let test_literals _ =
  check_typ Int (i 0);
  check_typ Int (i 123);
  check_typ Int (i (-1));
  check_typ UInt (iu 3);
  check_typ Long (il 3);
  check_typ ULong (iul 3);
  check_typ LongLong (ill 3);
  check_typ ULongLong (iull 3);
  check_typ Float (f 3.14);
  check_typ Double (d 3.14);
  check_typ LongDouble (ld 3.14);
  check_typ Bool (b true);
  check_typ Bool (b false);
  (* literals outside the 32-bit signed range are typed as long *)
  check_typ Long (i 2147483648);
  check_typ Long (i (-2147483649))

let test_float _ =
  let env = env_with [ ("f", Float); ("d", Double); ("ld", LongDouble) ] in
  check_typ ~env Float ("f" := f 3.14);
  check_typ ~env Float ("f" := d 3.14);
  check_typ ~env Float ("f" := ld 3.14);
  check_typ ~env Float ("f" := i 1);
  check_typ ~env Double ("d" := f 3.14);
  check_typ ~env Double ("d" := d 3.14);
  check_typ ~env Double ("d" := ld 3.14);
  check_typ ~env Double ("d" := i 1);
  check_typ ~env LongDouble ("ld" := f 3.14);
  check_typ ~env LongDouble ("ld" := d 3.14);
  check_typ ~env LongDouble ("ld" := ld 3.14);
  check_typ ~env LongDouble ("ld" := i 1);
  begin match typecheck_expr env ("f" := d 3.14) with
  | Assign (_, _, value) -> assert_implicit_cast_to Float value
  | _ -> assert_failure "expected float assignment cast"
  end;
  begin match typecheck_expr env ("d" := f 3.14) with
  | Assign (_, _, value) -> assert_implicit_cast_to Double value
  | _ -> assert_failure "expected double assignment cast"
  end;
  begin match typecheck_expr env ("ld" := d 3.14) with
  | Assign (_, _, value) -> assert_implicit_cast_to LongDouble value
  | _ -> assert_failure "expected long double assignment cast"
  end;
  begin match typecheck_expr env ("f" := i 1) with
  | Assign (_, _, value) -> assert_implicit_cast_to Float value
  | _ -> assert_failure "expected int-to-float assignment cast"
  end;
  begin match
    typecheck_stmt (default_env ()) (var_def ~init:(Some (i 1)) VFloat "f")
  with
  | VarDef { source_type = VFloat; init = Some init; _ } ->
      assert_equal ~printer:string_of_typ Float (expr_typ init)
  | _ -> assert_failure "expected float var def"
  end;
  begin match
    typecheck_stmt (default_env ()) (var_def ~init:(Some (f 1.0)) VDouble "d")
  with
  | VarDef { source_type = VDouble; init = Some init; _ } ->
      assert_equal ~printer:string_of_typ Double (expr_typ init)
  | _ -> assert_failure "expected double var def"
  end;
  begin match
    typecheck_stmt (default_env ())
      (var_def ~init:(Some (d 1.0)) VLongDouble "ld")
  with
  | VarDef { source_type = VLongDouble; init = Some init; _ } ->
      assert_equal ~printer:string_of_typ LongDouble (expr_typ init)
  | _ -> assert_failure "expected long double var def"
  end;
  let env =
    env_with_funcs
      [
        ("takes_float", { params = [ Float ]; ret = Void; is_variadic = false });
        ( "takes_double",
          { params = [ Double ]; ret = Void; is_variadic = false } );
        ( "takes_long_double",
          { params = [ LongDouble ]; ret = Void; is_variadic = false } );
      ]
  in
  check_typ ~env Void ("takes_float" $ [ f 1.0 ]);
  check_typ ~env Void ("takes_float" $ [ d 1.0 ]);
  check_typ ~env Void ("takes_float" $ [ i 1 ]);
  check_typ ~env Void ("takes_double" $ [ d 1.0 ]);
  check_typ ~env Void ("takes_double" $ [ f 1.0 ]);
  check_typ ~env Void ("takes_double" $ [ i 1 ]);
  check_typ ~env Void ("takes_long_double" $ [ ld 1.0 ]);
  check_typ ~env Void ("takes_long_double" $ [ d 1.0 ]);
  check_typ ~env Void ("takes_long_double" $ [ f 1.0 ]);
  begin match typecheck_expr env ("takes_float" $ [ d 1.0 ]) with
  | FuncCall (_, _, [ arg ]) -> assert_implicit_cast_to Float arg
  | _ -> assert_failure "expected float arg cast"
  end;
  begin match typecheck_expr env ("takes_double" $ [ f 1.0 ]) with
  | FuncCall (_, _, [ arg ]) -> assert_implicit_cast_to Double arg
  | _ -> assert_failure "expected double arg cast"
  end;
  begin match typecheck_expr env ("takes_long_double" $ [ d 1.0 ]) with
  | FuncCall (_, _, [ arg ]) -> assert_implicit_cast_to LongDouble arg
  | _ -> assert_failure "expected long double arg cast"
  end;
  begin match
    typecheck_stmt (default_env ()) (func_def VFloat "rf" [] [ ret (i 1) ])
  with
  | FuncDef { ret_type = VFloat; _ } -> ()
  | _ -> assert_failure "expected float return function"
  end;
  begin match
    typecheck_stmt (default_env ())
      (func_def VFloat "rf_cast" [] [ ret (d 1.0) ])
  with
  | FuncDef { body = [ ReturnStmt (_, Some value) ]; _ } ->
      assert_implicit_cast_to Float value
  | _ -> assert_failure "expected float return cast"
  end;
  begin match
    typecheck_stmt (default_env ()) (func_def VDouble "rd" [] [ ret (f 1.0) ])
  with
  | FuncDef { ret_type = VDouble; _ } -> ()
  | _ -> assert_failure "expected double return function"
  end;
  begin match
    typecheck_stmt (default_env ())
      (func_def VLongDouble "rld" [] [ ret (d 1.0) ])
  with
  | FuncDef { ret_type = VLongDouble; _ } -> ()
  | _ -> assert_failure "expected long double return function"
  end;
  begin match typecheck_expr (default_env ()) (bi Equal (i 1) (f 1.0)) with
  | BinaryOp (_, _, lhs, rhs) ->
      assert_implicit_cast_to Float lhs;
      assert_equal ~printer:string_of_typ Float (expr_typ rhs)
  | _ -> assert_failure "expected mixed float comparison"
  end;
  begin match typecheck_expr (default_env ()) (bi Equal (f 1.0) (d 1.0)) with
  | BinaryOp (_, _, lhs, rhs) ->
      assert_implicit_cast_to Double lhs;
      assert_equal ~printer:string_of_typ Double (expr_typ rhs)
  | _ -> assert_failure "expected float/double comparison"
  end;
  begin match typecheck_expr (default_env ()) (bi Equal (d 1.0) (ld 1.0)) with
  | BinaryOp (_, _, lhs, rhs) ->
      assert_implicit_cast_to LongDouble lhs;
      assert_equal ~printer:string_of_typ LongDouble (expr_typ rhs)
  | _ -> assert_failure "expected double/long double comparison"
  end;
  check_typ Float (bi Add (f 1.0) (f 2.0));
  check_typ Double (bi Add (d 1.0) (d 2.0));
  check_typ LongDouble (bi Add (ld 1.0) (d 2.0));
  check_typ Double (bi Mul (f 1.0) (d 2.0));
  check_err "operator '%': type mismatch between 'float' and 'float'"
    (bi Mod (f 1.0) (f 2.0));
  check_err "operator '<<': type mismatch between 'double' and 'int'"
    (bi LShift (d 1.0) (i 1));
  check_err "operator '&': type mismatch between 'float' and 'int'"
    (bi BitAnd (f 1.0) (i 1));
  check_typ Int (tern (f 1.0) (i 1) (i 2));
  check_typ Int (tern (d 1.0) (i 1) (i 2));
  check_typ Bool (bi And (f 1.0) (d 0.0));
  check_typ Bool (bi Or (d 0.0) (f 1.0));
  check_typ Double (tern (b true) (f 1.0) (d 1.0));
  check_typ Double (tern (b true) (d 1.0) (f 1.0));
  check_typ LongDouble (tern (b true) (ld 1.0) (d 1.0))

let test_arithmetic _ =
  check_typ Int (bi Add (i 1) (i 2));
  check_typ Int (bi Sub (i 5) (i 3));
  check_typ Int (bi Mul (i 3) (i 4));
  check_typ Int (bi Div (i 8) (i 2));
  check_typ Int (bi Mod (i 7) (i 3));
  check_typ Float (bi Add (f 1.0) (f 2.0));
  check_typ Double (bi Add (d 1.0) (d 2.0));
  check_typ Double (bi Add (i 1) (d 2.0));
  check_typ Double (bi Sub (d 5.0) (f 3.0));
  check_typ Float (bi Mul (f 3.0) (f 4.0));
  check_typ Double (bi Div (d 8.0) (i 2));
  check_typ LongDouble (bi Add (d 1.0) (ld 2.0));
  begin match typecheck_expr (default_env ()) (bi Add (i 1) (d 2.0)) with
  | BinaryOp (Checked (_, Double), Add, lhs, rhs) ->
      assert_implicit_cast_to Double lhs;
      assert_equal ~printer:string_of_typ Double (expr_typ rhs)
  | _ -> assert_failure "expected int/double arithmetic conversion"
  end;
  begin match typecheck_expr (default_env ()) (bi Mul (f 3.0) (i 4)) with
  | BinaryOp (Checked (_, Float), Mul, lhs, rhs) ->
      assert_equal ~printer:string_of_typ Float (expr_typ lhs);
      assert_implicit_cast_to Float rhs
  | _ -> assert_failure "expected int/float arithmetic conversion"
  end

let test_bitwise _ =
  check_typ Int (bi BitAnd (i 3) (i 5));
  check_typ Int (bi BitOr (i 3) (i 5));
  check_typ Int (bi BitXor (i 3) (i 5));
  check_typ Int (bi LShift (i 1) (i 2));
  check_typ Int (bi RShift (i 8) (i 1));
  let env = env_with [ ("l", Long); ("c", Char) ] in
  begin match typecheck_expr env (bi BitOr !"l" !"c") with
  | BinaryOp (Checked (_, Long), BitOr, lhs, rhs) ->
      assert_equal ~printer:string_of_typ Long (expr_typ lhs);
      assert_implicit_cast_to Long rhs
  | _ -> assert_failure "expected integer common-type conversion"
  end;
  begin match typecheck_expr env (bi LShift !"l" !"c") with
  | BinaryOp (Checked (_, Long), LShift, lhs, rhs) ->
      assert_equal ~printer:string_of_typ Long (expr_typ lhs);
      assert_implicit_cast_to Int rhs
  | _ -> assert_failure "expected shift promotions without common-type cast"
  end

let test_comparison _ =
  check_typ Bool (bi Less (i 1) (i 2));
  check_typ Bool (bi Leq (i 1) (i 2));
  check_typ Bool (bi Greater (i 2) (i 1));
  check_typ Bool (bi Geq (i 2) (i 1));
  check_typ Bool (bi Less (f 1.0) (f 2.0));
  check_typ Bool (bi Leq (d 1.0) (d 2.0));
  check_typ Bool (bi Greater (f 2.0) (d 1.0));
  check_typ Bool (bi Geq (i 2) (f 1.0))

let test_equality _ =
  check_typ Bool (bi Equal (i 1) (i 1));
  check_typ Bool (bi Neq (i 1) (i 2));
  check_typ Bool (bi Equal (b true) (b false));
  check_typ Bool (bi Neq (b true) (b true));
  check_typ Bool (bi Equal (f 1.0) (f 1.0));
  check_typ Bool (bi Neq (d 1.0) (d 2.0));
  check_typ Bool (bi Equal (ld 1.0) (d 1.0));
  check_typ Bool (bi Equal (i 1) (f 1.0));
  check_typ Bool (bi Neq (f 1.0) (d 2.0))

let test_logical _ =
  check_typ Bool (bi And (b true) (b false));
  check_typ Bool (bi Or (b false) (b true));
  (* && and || accept any scalar type *)
  check_typ Bool (bi And (i 1) (i 0));
  check_typ Bool (bi And (b true) (i 1));
  check_typ Bool (bi And (i 0) (b false));
  check_typ Bool (bi Or (i 1) (i 0));
  check_typ Bool (bi Or (b true) (i 1));
  check_typ Bool (bi Or (i 0) (b false))

let test_unary _ =
  let env = env_with [ ("pi", Ptr Int); ("pf", Ptr Float) ] in
  check_typ Bool (un Not (b true));
  check_typ Bool (un Not (b false));
  check_typ Bool (un Not (i 0));
  check_typ ~env Bool (un Not !"pi");
  check_typ ~env Bool (un Not !"pf");
  check_typ Bool (un Not (f 0.0));
  check_typ Bool (un Not (d 1.0));
  check_typ Bool (un Not (ld 1.0));
  check_typ Int (un Neg (i 1));
  check_typ Float (un Neg (f 1.0));
  check_typ Double (un Neg (d 1.0));
  check_typ LongDouble (un Neg (ld 1.0));
  check_typ Int (un Compl (i 0))

let test_unary_conversions _ =
  let env = env_with all_integer_vars in
  List.iter
    (fun t ->
      let expected = expected_promoted_type t in
      let name = name_of_typ t in
      let not_e = typecheck_expr env (un Not !name) in
      let neg = typecheck_expr env (un Neg !name) in
      let compl = typecheck_expr env (un Compl !name) in
      assert_equal ~printer:string_of_typ Bool (expr_typ not_e);
      assert_equal ~printer:string_of_typ expected (expr_typ neg);
      assert_equal ~printer:string_of_typ expected (expr_typ compl);
      if expected <> t then begin
        let neg_operand =
          match neg with
          | UnaryOp (_, _, operand) -> operand
          | _ -> assert_failure "expected UnaryOp"
        in
        let compl_operand =
          match compl with
          | UnaryOp (_, _, operand) -> operand
          | _ -> assert_failure "expected UnaryOp"
        in
        assert_implicit_cast_to expected neg_operand;
        assert_implicit_cast_to expected compl_operand
      end)
    all_integer_types

let test_nested _ =
  (* (1 + 2) * 3 *)
  check_typ Int (bi Mul (bi Add (i 1) (i 2)) (i 3));
  (* !true && false *)
  check_typ Bool (bi And (un Not (b true)) (b false));
  (* 1 < 2 == true *)
  check_typ Bool (bi Equal (bi Less (i 1) (i 2)) (b true))

let test_arithmetic_errors _ =
  check_err "operator '+': type mismatch between 'void' and 'int'"
    (bi Add noop (i 1));
  check_err "operator '*': type mismatch between 'bool' and 'void'"
    (bi Mul (b true) noop)

let test_comparison_errors _ =
  check_err "operator '<': type mismatch between 'void' and 'bool'"
    (bi Less noop (b false));
  check_err "operator '>=': type mismatch between 'int' and 'void'"
    (bi Geq (i 1) noop)

let test_equality_errors _ =
  check_err "operator '==': type mismatch between 'void' and 'int'"
    (bi Equal noop (i 1));
  check_err "operator '!=': type mismatch between 'bool' and 'void'"
    (bi Neq (b false) noop)

let test_logical_errors _ =
  (* void isn't scalar, so rejected by && and || *)
  check_err "operator '&&': type mismatch between 'void' and 'int'"
    (bi And noop (i 1));
  check_err "operator '||': type mismatch between 'bool' and 'void'"
    (bi Or (b true) noop)

let test_unary_errors _ =
  let env = env_with [ ("pi", Ptr Int); ("pf", Ptr Float) ] in
  check_err "operator '!': invalid operand type 'void'" (un Not noop);
  check_err "operator '-': invalid operand type 'void'" (un Neg noop);
  check_err "operator '~': invalid operand type 'void'" (un Compl noop);
  check_err "operator '~': invalid operand type 'float'" (un Compl (f 1.0));
  check_err "operator '~': invalid operand type 'double'" (un Compl (d 1.0));
  check_err "operator '~': invalid operand type 'long double'"
    (un Compl (ld 1.0));
  check_err ~env "operator '-': invalid operand type 'int*'" (un Neg !"pi");
  check_err ~env "operator '~': invalid operand type 'int*'" (un Compl !"pi");
  check_err ~env "operator '-': invalid operand type 'float*'" (un Neg !"pf");
  check_err ~env "operator '~': invalid operand type 'float*'" (un Compl !"pf")

let test_var_ref _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int !"x";
  check_typ ~env Bool !"ok"

let test_var_ref_errors _ =
  check_err "unbound variable 'x'" !"x";
  check_err "unbound variable 'y'" !"y"

let test_assign _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int ("x" := i 42);
  check_typ ~env Bool ("ok" := b true);
  check_typ ~env Int ("x" := bi Add (i 1) (i 2))

let test_assign_errors _ =
  let env = env_with [ ("x", Int) ] in
  check_err ~env "unbound variable 'y'" ("y" := i 1);
  check_err ~env "expected type 'int' but got 'void'" ("x" := noop)

let test_func_call _ =
  let env =
    env_with_funcs
      [
        ("add", { params = [ Int; Int ]; ret = Int; is_variadic = false });
        ("ready", { params = []; ret = Bool; is_variadic = false });
      ]
  in
  check_typ ~env Int ("add" $ [ i 1; i 2 ]);
  check_typ ~env Bool ("ready" $ [])

let test_func_call_errors _ =
  let env =
    env_with_funcs
      [ ("add", { params = [ Int; Int ]; ret = Int; is_variadic = false }) ]
  in
  check_err ~env "unbound function 'f'" ("f" $ []);
  check_err ~env "'add' expects 2 argument(s) but got 1" ("add" $ [ i 1 ]);
  check_err ~env "'add' expects 2 argument(s) but got 0" ("add" $ []);
  check_err ~env "expected type 'int' but got 'void'" ("add" $ [ noop; i 1 ])

let test_ternary _ =
  check_typ Int (tern (b true) (i 1) (i 2));
  (* bool branches both promote to int *)
  check_typ Int (tern (b false) (b true) (b false));
  check_typ Int (tern (bi Less (i 1) (i 2)) (b true) (b false));
  (* scalar is coerced to bool *)
  check_typ Int (tern (i 1) (i 2) (i 3));
  (* promote to the larger type *)
  let env = env_with [ ("c", Char); ("l", Long) ] in
  check_typ ~env Int (tern (b true) !"c" (i 2));
  check_typ ~env Long (tern (b true) (i 1) !"l");
  (* bool promotes to int *)
  check_typ Int (tern (b true) (b false) (i 1));
  check_typ Int (tern (b true) (i 1) (b false));
  check_typ Double (tern (b true) (f 1.0) (d 2.0));
  check_typ Double (tern (b true) (i 1) (d 2.0));
  begin match typecheck_expr (default_env ()) (tern (b true) (i 1) (d 2.0)) with
  | Ternary (Checked (_, Double), _, then_e, else_e) ->
      assert_implicit_cast_to Double then_e;
      assert_equal ~printer:string_of_typ Double (expr_typ else_e)
  | _ -> assert_failure "expected arithmetic ternary conversion"
  end

let test_array_decay_exprs _ =
  let env =
    env_with
      [ ("a2", Array (Char, 2)); ("a3", Array (Char, 3)); ("p", Ptr Char) ]
  in
  begin match typecheck_expr env (tern (b true) !"a2" !"a3") with
  | Ternary (Checked (_, Ptr Char), _, then_e, else_e) ->
      assert_implicit_cast_to (Ptr Char) then_e;
      assert_implicit_cast_to (Ptr Char) else_e
  | _ -> assert_failure "expected array ternary to decay to char*"
  end;
  begin match typecheck_expr env (tern (b true) !"a2" !"p") with
  | Ternary (Checked (_, Ptr Char), _, then_e, else_e) ->
      assert_implicit_cast_to (Ptr Char) then_e;
      assert_equal ~printer:string_of_typ (Ptr Char) (expr_typ else_e)
  | _ -> assert_failure "expected array/pointer ternary to have char* type"
  end;
  begin match typecheck_expr env (bi Equal !"a2" !"a3") with
  | BinaryOp (Checked (_, Bool), Equal, lhs, rhs) ->
      assert_implicit_cast_to (Ptr Char) lhs;
      assert_implicit_cast_to (Ptr Char) rhs
  | _ -> assert_failure "expected array equality to decay both sides"
  end;
  begin match typecheck_expr env (bi Neq !"a2" !"p") with
  | BinaryOp (Checked (_, Bool), Neq, lhs, rhs) ->
      assert_implicit_cast_to (Ptr Char) lhs;
      assert_equal ~printer:string_of_typ (Ptr Char) (expr_typ rhs)
  | _ -> assert_failure "expected array/pointer equality to decay array side"
  end;
  begin match typecheck_expr env (bi Less !"a2" !"a3") with
  | BinaryOp (Checked (_, Bool), Less, lhs, rhs) ->
      assert_implicit_cast_to (Ptr Char) lhs;
      assert_implicit_cast_to (Ptr Char) rhs
  | _ -> assert_failure "expected array comparison to decay both sides"
  end;
  begin match
    typecheck_expr (default_env ()) (bi Equal (s [ 97 ]) (s [ 97; 98 ]))
  with
  | BinaryOp (Checked (_, Bool), Equal, lhs, rhs) ->
      assert_implicit_cast_to (Ptr Char) lhs;
      assert_implicit_cast_to (Ptr Char) rhs
  | _ ->
      assert_failure
        "expected different-length string literal comparison to decay both \
         sides"
  end;
  begin match typecheck_expr env (un Not !"a2") with
  | UnaryOp (Checked (_, Bool), Not, operand) ->
      assert_implicit_cast_to (Ptr Char) operand
  | _ -> assert_failure "expected unary ! to decay arrays to pointers"
  end;
  begin match typecheck_expr env (bi And !"a2" !"p") with
  | BinaryOp (Checked (_, Bool), And, lhs, rhs) ->
      assert_implicit_cast_to Bool lhs;
      assert_implicit_cast_to Bool rhs
  | _ -> assert_failure "expected logical && to accept decayed array operands"
  end;
  (* arr + 1: array decays to char*, enabling pointer arithmetic *)
  begin match typecheck_expr env (bi Add !"a2" (i 1)) with
  | BinaryOp (Checked (_, Ptr Char), Add, lhs, _) ->
      assert_implicit_cast_to (Ptr Char) lhs
  | _ -> assert_failure "expected arr + 1 to decay arr and give char*"
  end;
  (* &arr: AddrOf skips decay, giving pointer-to-array not pointer-to-pointer *)
  begin match typecheck_expr env (un AddrOf !"a2") with
  | UnaryOp (Checked (_, Ptr (Array (Char, 2))), AddrOf, _) -> ()
  | _ ->
      assert_failure
        "expected &arr to give pointer-to-array not pointer-to-pointer"
  end;
  (* ternary with incompatible array types: both decay but int* != char* *)
  let env2 = env_with [ ("ai", Array (Int, 3)); ("ac", Array (Char, 3)) ] in
  check_err ~env:env2 "expected type 'int*' but got 'char*'"
    (tern (b true) !"ai" !"ac")

let test_array_decay_stmts _ =
  let arr_env = env_with [ ("arr", Array (Char, 3)); ("vp", Ptr Void) ] in
  (* var init: char* p2 = arr decays *)
  begin match
    typecheck_stmt arr_env (var_def ~init:(Some !"arr") (VPtr VChar) "p2")
  with
  | VarDef { init = Some init; _ } -> assert_implicit_cast_to (Ptr Char) init
  | _ -> assert_failure "expected array var init to decay to char*"
  end;
  (* var init: void* vp2 = arr -- array decays to char*, then converts to
     void* *)
  begin match
    typecheck_stmt arr_env (var_def ~init:(Some !"arr") (VPtr VVoid) "vp2")
  with
  | VarDef { init = Some init; _ } -> assert_implicit_cast_to (Ptr Void) init
  | _ -> assert_failure "expected array var init to decay and convert to void*"
  end;
  (* assign: vp = arr decays to char*, then converts to void* *)
  begin match typecheck_expr arr_env ("vp" := !"arr") with
  | Assign (Checked (_, Ptr Void), _, rhs) ->
      assert_implicit_cast_to (Ptr Void) rhs
  | _ -> assert_failure "expected array assign to void* to decay and convert"
  end;
  (* func call: char* and void* params both accept array args *)
  let call_env =
    let e = env_with [ ("arr", Array (Char, 3)) ] in
    Hashtbl.add e.funcs "f_char"
      { params = [ Ptr Char ]; ret = Void; is_variadic = false };
    Hashtbl.add e.funcs "f_void"
      { params = [ Ptr Void ]; ret = Void; is_variadic = false };
    e
  in
  begin match typecheck_expr call_env ("f_char" $ [ !"arr" ]) with
  | FuncCall (_, _, [ arg ]) -> assert_implicit_cast_to (Ptr Char) arg
  | _ -> assert_failure "expected array arg to char* param to decay"
  end;
  begin match typecheck_expr call_env ("f_void" $ [ !"arr" ]) with
  | FuncCall (_, _, [ arg ]) -> assert_implicit_cast_to (Ptr Void) arg
  | _ -> assert_failure "expected array arg to void* param to decay and convert"
  end;
  (* return: array decays then converts to void* return type *)
  let fn_env =
    {
      (env_with [ ("arr", Array (Char, 3)) ]) with
      return_typ = Some (Ptr Void);
    }
  in
  begin match typecheck_stmt fn_env (ret !"arr") with
  | ReturnStmt (_, Some ret_e) -> assert_implicit_cast_to (Ptr Void) ret_e
  | _ -> assert_failure "expected return array to decay and convert to void*"
  end;
  (* sizeof: array is not decayed, full array size is preserved *)
  begin match
    typecheck_expr
      (env_with [ ("arr", Array (Char, 3)) ])
      (SizeofExpr (p, !"arr"))
  with
  | SizeofExpr (Checked (_, Long), inner) ->
      assert_equal ~printer:string_of_typ (Array (Char, 3)) (expr_typ inner)
  | _ -> assert_failure "expected sizeof array to not decay"
  end;
  (* wrong element type: int arr[3] cannot decay to char* *)
  let int_arr_env = env_with [ ("iarr", Array (Int, 3)) ] in
  check_stmt_err ~env:int_arr_env "expected type 'char*' but got 'int*'"
    (var_def ~init:(Some !"iarr") (VPtr VChar) "p")

let test_ternary_errors _ =
  (* void is not a scalar condition *)
  check_err "condition must be 'bool' but got 'void'" (tern noop (i 1) (i 2));
  (* not an integer type, so mismatch *)
  check_err "expected type 'int' but got 'void'" (tern (b true) (i 1) noop);
  check_err "expected type 'void' but got 'int'" (tern (b true) noop (i 1))

let test_incdec _ =
  let env =
    env_with [ ("x", Int); ("flag", Bool); ("f", Float); ("d", Double) ]
  in
  check_typ ~env Int (pre_inc !"x");
  check_typ ~env Int (post_inc !"x");
  check_typ ~env Int (pre_dec !"x");
  check_typ ~env Int (post_dec !"x");
  check_typ ~env Bool (pre_inc !"flag");
  check_typ ~env Bool (post_inc !"flag");
  check_typ ~env Bool (pre_dec !"flag");
  check_typ ~env Bool (post_dec !"flag");
  check_typ ~env Float (pre_inc !"f");
  check_typ ~env Float (post_inc !"f");
  check_typ ~env Float (pre_dec !"f");
  check_typ ~env Float (post_dec !"f");
  check_typ ~env Double (pre_inc !"d");
  check_typ ~env Double (post_inc !"d");
  check_typ ~env Double (pre_dec !"d");
  check_typ ~env Double (post_dec !"d");
  (* inc/dec through a dereferenced pointer lvalue *)
  let env = env_with [ ("p", Ptr Int); ("pp", Ptr (Ptr Int)) ] in
  check_typ ~env Int (pre_inc (un Deref !"p"));
  check_typ ~env Int (post_inc (un Deref !"p"));
  check_typ ~env Int (pre_dec (un Deref !"p"));
  check_typ ~env Int (post_dec (un Deref !"p"));
  (* inc/dec through a double-pointer dereference *)
  check_typ ~env (Ptr Int) (pre_inc (un Deref !"pp"));
  check_typ ~env (Ptr Int) (post_inc (un Deref !"pp"));
  check_typ ~env (Ptr Int) (pre_dec (un Deref !"pp"));
  check_typ ~env (Ptr Int) (post_dec (un Deref !"pp"))

let test_incdec_errors _ =
  let env_int = env_with [ ("x", Int) ] in
  (* non-lvalue operand *)
  check_err ~env:env_int "expression is not an lvalue" (pre_inc (i 1));
  check_err ~env:env_int "expression is not an lvalue"
    (post_inc (bi Add !"x" (i 1)));
  check_err "expression is not an lvalue" (pre_inc noop);
  check_err "expression is not an lvalue" (post_dec noop);
  (* inc/dec on void ptr dereference is not allowed *)
  let env_vp = env_with [ ("vp", Ptr Void) ] in
  check_err ~env:env_vp "operator '*': invalid operand type 'void*'"
    (pre_inc (un Deref !"vp"))

let test_cast _ =
  let env = env_with [ ("x", Long); ("n", Int); ("c", Char); ("p", Ptr Int) ] in
  check_typ ~env Int (cast VInt !"x");
  check_typ ~env Long (cast VLong !"n");
  check_typ ~env Float (cast VFloat !"n");
  check_typ ~env Double (cast VDouble !"n");
  check_typ ~env LongDouble (cast VLongDouble !"n");
  check_typ ~env Bool (cast VBool !"n");
  check_typ ~env Char (cast VChar !"n");
  check_typ ~env (Ptr Int) (cast (VPtr VInt) !"n");
  check_typ ~env Int (cast VInt !"p");
  check_typ ~env (Ptr (Ptr Int)) (cast (VPtr (VPtr VInt)) !"p");
  check_typ Int (cast VInt (i 5));
  check_typ Long (cast VLong (i 5));
  check_typ Float (cast VFloat (i 5));
  check_typ Double (cast VDouble (i 5));
  check_typ LongDouble (cast VLongDouble (i 5));
  check_typ Int (cast VInt (f 1.0));
  check_typ Int (cast VInt (d 1.0));
  check_typ Double (cast VDouble (f 1.0));
  check_typ Float (cast VFloat (d 1.0));
  check_typ LongDouble (cast VLongDouble (d 1.0));
  check_typ Double (cast VDouble (ld 1.0))

let test_cast_errors _ =
  check_err "cannot cast from 'int' to 'void'" (cast VVoid (i 1));
  check_err "cannot cast from 'void' to 'int'" (cast VInt noop)

let test_conversions _ =
  let env =
    env_with
      [
        ("c", Char);
        ("uc", UChar);
        ("s", Short);
        ("us", UShort);
        ("u", UInt);
        ("l", Long);
        ("ul", ULong);
        ("ll", LongLong);
      ]
  in
  check_typ ~env Int (bi Add !"c" (i 1));
  check_typ ~env Int (bi Add !"uc" !"s");
  check_typ ~env Int (bi Add !"us" !"s");
  check_typ ~env UInt (bi Add !"u" (i 1));
  check_typ ~env Long (bi Add !"l" !"u");
  check_typ ~env ULong (bi Add !"ul" !"l");
  check_typ ~env LongLong (bi Add !"ll" !"u");
  check_typ ~env ULongLong (bi Add !"ll" !"ul");
  check_typ ~env Int (bi LShift !"c" (i 1));
  check_typ ~env Int (bi RShift !"us" (b false));
  check_typ Bool (bi Equal (b true) (i 1));
  check_typ ~env Bool (bi Less !"ul" !"ll");
  check_typ Int (un Neg (b true));
  check_typ Int (un Compl (b false));
  let env = env_with [ ("ok", Bool) ] in
  check_typ ~env Bool ("ok" := i 7);
  let env =
    env_with_funcs
      [
        ("flag", { params = [ Bool ]; ret = Bool; is_variadic = false });
        ("widen", { params = [ Long ]; ret = Long; is_variadic = false });
      ]
  in
  check_typ ~env Bool ("flag" $ [ i 1 ]);
  check_typ ~env Long ("widen" $ [ i 1 ]);
  check_typ Int (cast VInt (b true));
  check_typ Bool (cast VBool (i 7));
  check_typ ULongLong (cast VULongLong (i 7))

let test_int_promotion _ =
  (* small integer types promote to int before binary operations *)
  let env =
    env_with [ ("c", Char); ("uc", UChar); ("s", Short); ("us", UShort) ]
  in
  check_typ ~env Int (bi Add !"c" !"c");
  check_typ ~env Int (bi Add !"uc" !"uc");
  check_typ ~env Int (bi Add !"s" !"s");
  check_typ ~env Int (bi Add !"us" !"us");
  (* bool promotes to int too *)
  check_typ Int (bi Add (b true) (b false));
  (* verify the promoted type in the AST *)
  match typecheck_expr env (bi Add !"c" !"c") with
  | BinaryOp (Checked (_, Int), Add, lhs, rhs) ->
      assert_implicit_cast_to Int lhs;
      assert_implicit_cast_to Int rhs
  | _ -> assert_failure "expected BinaryOp with Int result and cast operands"

let test_sign_zero_ext _ =
  (* signed values are widened *)
  let env =
    env_with [ ("sc", Char); ("uc", UChar); ("ss", Short); ("us", UShort) ]
  in
  (match typecheck_expr env (bi Add !"sc" (i 0)) with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Int lhs
  | _ -> assert_failure "expected BinaryOp");
  (match typecheck_expr env (bi Add !"uc" (i 0)) with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Int lhs
  | _ -> assert_failure "expected BinaryOp");
  (match typecheck_expr env (bi Add !"ss" (i 0)) with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Int lhs
  | _ -> assert_failure "expected BinaryOp");
  match typecheck_expr env (bi Add !"us" (i 0)) with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Int lhs
  | _ -> assert_failure "expected BinaryOp"

let test_implicit_casts _ =
  let env = env_with [ ("ok", Bool); ("c", Char); ("l", Long) ] in
  (match typecheck_expr env ("ok" := i 3) with
  | Assign (_, _, value) -> assert_implicit_cast_to Bool value
  | _ -> assert_failure "expected Assign");
  match typecheck_expr env (bi Add !"c" !"l") with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Long lhs
  | _ -> assert_failure "expected BinaryOp"

let test_pointers _ =
  let env =
    env_with_all
      [
        ("x", Int);
        ("y", Int);
        ("px", Ptr Int);
        ("py", Ptr Int);
        ("pp", Ptr (Ptr Int));
      ]
      [ ("load", { params = [ Ptr Int ]; ret = Int; is_variadic = false }) ]
  in
  check_typ ~env (Ptr Int) (un AddrOf !"x");
  check_typ ~env Int (un Deref !"px");
  check_typ ~env (Ptr (Ptr Int)) (un AddrOf (un Deref !"pp"));
  check_typ ~env Bool (bi Equal !"px" !"py");
  check_typ ~env (Ptr Int) ("px" := un AddrOf !"x");
  check_typ ~env Int ("load" $ [ un AddrOf !"x" ]);
  (* assign through a pointer *)
  check_typ ~env Int (Assign (p, un Deref !"px", i 42));
  check_typ ~env Int (Assign (p, un Deref !"px", !"x"));
  (* assign through a double pointer *)
  check_typ ~env (Ptr Int) (Assign (p, un Deref !"pp", !"px"))

let test_pointer_arithmetic _ =
  let env =
    env_with
      [
        ("x", Int);
        ("n", Int);
        ("c", Char);
        ("l", Long);
        ("px", Ptr Int);
        ("py", Ptr Int);
        ("pc", Ptr Char);
        ("pp", Ptr (Ptr Int));
      ]
  in
  (* ptr + int, int + ptr *)
  check_typ ~env (Ptr Int) (bi Add !"px" !"n");
  check_typ ~env (Ptr Int) (bi Add !"n" !"px");
  (* ptr + non-Int integer offset types *)
  check_typ ~env (Ptr Int) (bi Add !"px" !"c");
  check_typ ~env (Ptr Int) (bi Add !"px" !"l");
  check_typ ~env (Ptr Int) (bi Add !"c" !"px");
  check_typ ~env (Ptr Int) (bi Add !"l" !"px");
  (* ptr - int *)
  check_typ ~env (Ptr Int) (bi Sub !"px" !"n");
  check_typ ~env (Ptr Int) (bi Sub !"px" !"c");
  check_typ ~env (Ptr Int) (bi Sub !"px" !"l");
  (* ptr - ptr -> long *)
  check_typ ~env Long (bi Sub !"px" !"py");
  check_typ ~env Long (bi Sub !"pc" !"pc");
  (* pointer comparisons *)
  check_typ ~env Bool (bi Less !"px" !"py");
  check_typ ~env Bool (bi Leq !"px" !"py");
  check_typ ~env Bool (bi Greater !"px" !"py");
  check_typ ~env Bool (bi Geq !"px" !"py");
  check_typ ~env Bool (bi Equal !"px" !"py");
  check_typ ~env Bool (bi Neq !"px" !"py");
  (* ptr++ and ++ptr on int* *)
  check_typ ~env (Ptr Int) (post_inc !"px");
  check_typ ~env (Ptr Int) (pre_inc !"px");
  check_typ ~env (Ptr Int) (post_dec !"px");
  check_typ ~env (Ptr Int) (pre_dec !"px");
  (* ptr++ on a double pointer *)
  check_typ ~env (Ptr (Ptr Int)) (post_inc !"pp");
  check_typ ~env (Ptr (Ptr Int)) (pre_inc !"pp")

let test_pointer_arithmetic_errors _ =
  let env =
    env_with
      [
        ("x", Int);
        ("px", Ptr Int);
        ("py", Ptr Int);
        ("pc", Ptr Char);
        ("pp", Ptr (Ptr Int));
      ]
  in
  (* int - ptr is invalid *)
  check_err ~env "operator '-': type mismatch between 'int' and 'int*'"
    (bi Sub !"x" !"px");
  (* ptr + ptr is invalid *)
  check_err ~env "operator '+': type mismatch between 'int*' and 'int*'"
    (bi Add !"px" !"py");
  (* ptr - ptr of different types is invalid *)
  check_err ~env "operator '-': type mismatch between 'int*' and 'char*'"
    (bi Sub !"px" !"pc");
  (* ptr * int is invalid *)
  check_err ~env "operator '*': type mismatch between 'int*' and 'int'"
    (bi Mul !"px" !"x");
  (* pointer comparisons between different pointer types are invalid *)
  check_err ~env "operator '<': type mismatch between 'int*' and 'char*'"
    (bi Less !"px" !"pc");
  check_err ~env "operator '==': type mismatch between 'int*' and 'char*'"
    (bi Equal !"px" !"pc");
  ()

let test_void_ptr _ =
  let env =
    env_with_all
      [ ("p", Ptr Int); ("vp", Ptr Void); ("n", Int) ]
      [
        ("take_vp", { params = [ Ptr Void ]; ret = Void; is_variadic = false });
        ("take_p", { params = [ Ptr Int ]; ret = Void; is_variadic = false });
        ("noop", { params = []; ret = Void; is_variadic = false });
      ]
  in
  (* void* to/from T* implicit conversion in assignment *)
  check_typ ~env (Ptr Void) ("vp" := !"p");
  check_typ ~env (Ptr Int) ("p" := !"vp");
  (* void* <-> T* implicit conversion in function arguments *)
  check_typ ~env Void ("take_vp" $ [ !"p" ]);
  check_typ ~env Void ("take_p" $ [ !"vp" ]);
  (* void* arithmetic is rejected *)
  check_err ~env "operator '+': type mismatch between 'void*' and 'int'"
    (bi Add !"vp" !"n");
  check_err ~env "operator '+': type mismatch between 'int' and 'void*'"
    (bi Add !"n" !"vp");
  check_err ~env "operator '-': type mismatch between 'void*' and 'int'"
    (bi Sub !"vp" !"n");
  check_err ~env "operator '-': type mismatch between 'void*' and 'void*'"
    (bi Sub !"vp" !"vp")

let test_null_ptr_constant _ =
  let env = env_with [ ("p", Ptr Int); ("vp", Ptr Void) ] in
  (* integer 0 is a null pointer constant: assigns to any pointer type *)
  check_typ ~env (Ptr Int) ("p" := i 0);
  check_typ ~env (Ptr Void) ("vp" := i 0);
  (* null pointer comparisons: both orderings *)
  check_typ ~env Bool (bi Equal !"p" (i 0));
  check_typ ~env Bool (bi Neq !"p" (i 0));
  check_typ ~env Bool (bi Equal (i 0) !"p");
  check_typ ~env Bool (bi Neq (i 0) !"vp");
  (* non-zero integer literals are not null pointer constants *)
  check_err ~env "expected type 'int*' but got 'int'" ("p" := i 1);
  check_err ~env "expected type 'int*' but got 'int'" ("p" := i 42);
  check_err ~env "operator '==': type mismatch between 'int*' and 'int'"
    (bi Equal !"p" (i 1));
  check_err ~env "operator '!=': type mismatch between 'int*' and 'int'"
    (bi Neq !"p" (i 42))

let test_pointer_errors _ =
  let env =
    env_with [ ("x", Int); ("p", Ptr Int); ("vp", Ptr Void); ("q", Ptr Int) ]
  in
  check_err ~env "expression is not an lvalue" (un AddrOf (bi Add !"x" (i 1)));
  check_err ~env "operator '*': invalid operand type 'int'" (un Deref !"x");
  check_err ~env "operator '*': invalid operand type 'void*'" (un Deref !"vp");
  check_err ~env "expected type 'int*' but got 'int'" ("p" := i 1);
  check_err ~env "expected type 'int*' but got 'bool'" ("p" := b true);
  (* assign wrong type through pointer: int* cannot be implicitly cast to int *)
  check_err ~env "expected type 'int' but got 'int*'"
    (Assign (p, un Deref !"p", !"p"))

let test_stmt_conditions _ =
  (* any scalar can be used as a condition in control flow stmts *)
  let ok s = ignore (typecheck_stmt (default_env ()) s) in
  let ok_env env s = ignore (typecheck_stmt env s) in
  ok (if_ (i 1) empty);
  ok (if_ (f 1.0) empty);
  ok (if_ (d 0.0) empty);
  ok (while_ (i 1) empty);
  ok (do_while_ empty (i 1));
  ok (for_ ~cond:(Some (i 1)) empty empty);
  let arr_env = env_with [ ("arr", Array (Char, 3)) ] in
  ok_env arr_env (if_ !"arr" empty);
  ok_env arr_env (while_ !"arr" empty);
  ok_env arr_env (do_while_ empty !"arr");
  ok_env arr_env (for_ ~cond:(Some !"arr") empty empty);
  (* void is not scalar, so void conditions are still rejected *)
  check_stmt_err "condition must be 'bool' but got 'void'" (if_ noop empty);
  check_stmt_err "condition must be 'bool' but got 'void'" (while_ noop empty);
  check_stmt_err "condition must be 'bool' but got 'void'"
    (do_while_ empty noop);
  check_stmt_err "condition must be 'bool' but got 'void'"
    (for_ ~cond:(Some noop) empty empty)

let test_break_continue _ =
  (* break and continue are valid inside loops *)
  let loop_env = { (default_env ()) with in_loop = true } in
  (match typecheck_stmt loop_env (BreakStmt dummy_pos) with
  | BreakStmt _ -> ()
  | _ -> assert_failure "expected BreakStmt");
  match typecheck_stmt loop_env (ContinueStmt dummy_pos) with
  | ContinueStmt _ -> ()
  | _ -> assert_failure "expected ContinueStmt"

let test_var_type_resolution _ =
  let check vt = ignore (typecheck_stmt (default_env ()) (var_def vt "x")) in
  check VBool;
  check VChar;
  check VSChar;
  check VUChar;
  check VShort;
  check VUShort;
  check VInt;
  check VUInt;
  check VLong;
  check VULong;
  check VLongLong;
  check VULongLong;
  check (VPtr VInt);
  check (VPtr (VPtr VInt));
  (* VNamed with an unknown name must raise UnknownType *)
  check_stmt_err "unknown type 'Foo'" (var_def (VNamed "Foo") "x")

let test_typedefs _ =
  let env =
    let env = default_env () in
    { env with typedefs = [ make_tbl [ ("myint", VInt) ] ] }
  in
  check_typ ~env Int (cast (VNamed "myint") (i 7));
  let env = default_env () in
  begin match
    typecheck_stmt env
      (Typedef
         {
           pos = dummy_pos;
           struct_def = None;
           existing_type = VInt;
           alias = "myint";
         })
  with
  | Typedef { existing_type = VInt; alias = "myint"; _ } -> ()
  | _ -> assert_failure "expected checked typedef"
  end;
  ignore (typecheck_stmt env (var_def (VNamed "myint") "x"));
  ignore
    (typecheck_stmt env
       (Typedef
          {
            pos = dummy_pos;
            struct_def = None;
            existing_type = VArray (VInt, 10);
            alias = "arr";
          }));
  match
    typecheck_stmt
      (let env = default_env () in
       { env with typedefs = [ make_tbl [ ("arr", VArray (VInt, 10)) ] ] })
      (var_def (VNamed "arr") "xs")
  with
  | VarDef { name = "xs"; source_type = VArray (VInt, 10); init = None; _ } ->
      ()
  | _ -> assert_failure "expected array typedef var def"

let test_typedef_scope _ =
  let env = default_env () in
  ignore
    (typecheck_stmt env
       (CompoundStmt
          ( dummy_pos,
            [
              Typedef
                {
                  pos = dummy_pos;
                  struct_def = None;
                  existing_type = VInt;
                  alias = "myint";
                };
            ] )));
  check_stmt_err "unknown type 'myint'" (var_def (VNamed "myint") "x")

let test_typedef_cycle _ =
  let env =
    let env = default_env () in
    {
      env with
      typedefs = [ make_tbl [ ("a", VNamed "b"); ("b", VNamed "a") ] ];
    }
  in
  check_stmt_err ~env "unknown type 'a'" (var_def (VNamed "a") "x")

let test_array_param_decay _ =
  let env =
    let env = default_env () in
    { env with typedefs = [ make_tbl [ ("numbers", VArray (VInt, 4)) ] ] }
  in
  match
    typecheck_stmt env
      (func_def VInt "sum" [ (VNamed "numbers", "values") ] [ ret (i 0) ])
  with
  | FuncDef { params = [ (VPtr VInt, "values") ]; _ } -> ()
  | _ -> assert_failure "expected array parameter to decay to int*"

let test_break_continue_errors _ =
  (* break and continue outside a loop are errors *)
  check_stmt_err "break statement outside of a loop" (BreakStmt dummy_pos);
  check_stmt_err "continue statement outside of a loop" (ContinueStmt dummy_pos)

let test_missing_return _ =
  check_stmt_err "control reaches end of non-void function 'f'"
    (func_def VInt "f" [] [ empty ]);
  ignore
    (typecheck_stmt (default_env ())
       (func_def VInt "f" []
          [ if_ ~else_:(Some (ret (i 2))) (b true) (ret (i 1)) ]))

let test_typecheck_program _ =
  let program =
    [
      var_def ~init:(Some (i 1)) VInt "x";
      ExprStmt (dummy_pos, FuncCall (p, "printint", [ !"x" ]));
    ]
  in
  match typecheck_program program with
  | [
   VarDef { name = "x"; init = Some init; _ };
   ExprStmt (_, FuncCall (Checked (_, Void), "printint", [ VarRef (_, "x") ]));
  ] -> assert_equal ~printer:string_of_typ Int (expr_typ init)
  | _ -> assert_failure "expected typed program using stdlib printint"

let test_func_decl _ =
  begin match
    typecheck_stmt (default_env ())
      (func_decl ~is_extern:true VInt "puts" [ (VPtr VChar, "s") ])
  with
  | FuncDecl
      { ret_type = VInt; params = [ (VPtr VChar, "s") ]; is_extern = true; _ }
    -> ()
  | _ -> assert_failure "expected checked func decl"
  end;
  let env = default_env () in
  ignore
    (typecheck_stmt env
       (func_decl ~is_extern:true VInt "puts" [ (VPtr VChar, "s") ]));
  check_typ ~env Int ("puts" $ [ StringLiteral (p, [ 104; 105 ]) ])

let test_variadic_func_call _ =
  let env =
    env_with_funcs
      [ ("printf", { params = [ Ptr Char ]; ret = Int; is_variadic = true }) ]
  in
  check_typ ~env Int ("printf" $ [ StringLiteral (p, [ 37; 100 ]); i 3 ]);
  check_err ~env "'printf' expects 1 argument(s) but got 0" ("printf" $ []);
  begin match
    typecheck_expr env ("printf" $ [ StringLiteral (p, [ 37; 102 ]); f 1.5 ])
  with
  | FuncCall (_, _, [ _; ImplicitCast (Checked (_, Double), _, _) ]) -> ()
  | _ -> assert_failure "expected float variadic arg to promote to double"
  end;
  (* array variable as variadic arg: decays to char* *)
  let env2 =
    let e = env_with [ ("arr", Array (Char, 3)) ] in
    Hashtbl.add e.funcs "printf"
      { params = [ Ptr Char ]; ret = Int; is_variadic = true };
    e
  in
  begin match
    typecheck_expr env2 ("printf" $ [ StringLiteral (p, [ 37; 115 ]); !"arr" ])
  with
  | FuncCall (_, _, [ _; ImplicitCast (Checked (_, Ptr Char), _, _) ]) -> ()
  | _ -> assert_failure "expected array variadic arg to decay to char*"
  end

let test_subscript _ =
  let env =
    env_with [ ("arr", Array (Int, 4)); ("pc", Ptr Char); ("vp", Ptr Void) ]
  in
  (* array subscript gives element type *)
  check_typ ~env Int (Subscript (p, !"arr", i 0));
  (* pointer subscript gives element type *)
  check_typ ~env Char (Subscript (p, !"pc", i 1));
  (* long index is valid *)
  check_typ ~env Int (Subscript (p, !"arr", il 0));
  (* subscript result is an lvalue and can be assigned to *)
  check_typ ~env Int (Assign (p, Subscript (p, !"arr", i 0), i 99));
  (* non-integer index fails *)
  check_err ~env "operator '*': invalid operand type 'float'"
    (Subscript (p, !"arr", f 1.0));
  (* void pointer subscript fails *)
  check_err ~env "operator '*': invalid operand type 'void*'"
    (Subscript (p, !"vp", i 0));
  (* subscript on a plain non-pointer type fails *)
  check_err "operator '*': invalid operand type 'int'" (Subscript (p, i 0, i 0))

let test_sizeof _ =
  (* SizeofType: all primitive types return Long *)
  check_typ Long (SizeofType (p, VInt));
  check_typ Long (SizeofType (p, VChar));
  check_typ Long (SizeofType (p, VBool));
  check_typ Long (SizeofType (p, VLong));
  check_typ Long (SizeofType (p, VPtr VInt));
  check_typ Long (SizeofType (p, VArray (VChar, 3)));
  (* SizeofExpr: expression type is always Long regardless of operand type *)
  check_typ Long (SizeofExpr (p, i 1));
  check_typ Long (SizeofExpr (p, b true));
  let penv = env_with [ ("px", Ptr Int) ] in
  check_typ ~env:penv Long (SizeofExpr (p, !"px"));
  (* SizeofType on a struct type *)
  let senv =
    let env = default_env () in
    Hashtbl.add env.structs "Foo" [ ("x", Int) ];
    env
  in
  check_typ ~env:senv Long (SizeofType (p, VStruct "Foo"));
  (* SizeofExpr does not decay array: inner node retains array type *)
  begin match
    typecheck_expr
      (env_with [ ("arr", Array (Char, 3)) ])
      (SizeofExpr (p, !"arr"))
  with
  | SizeofExpr (Checked (_, Long), inner) ->
      assert_equal ~printer:string_of_typ (Array (Char, 3)) (expr_typ inner)
  | _ -> assert_failure "expected sizeof array to preserve array type"
  end

let test_struct_def _ =
  (* StructDef registers the struct in env.structs *)
  let env = default_env () in
  ignore
    (typecheck_stmt env
       (StructDef
          {
            pos = dummy_pos;
            tag = "Point";
            fields = [ (VInt, "x"); (VInt, "y") ];
            var_name = None;
          }));
  begin match lookup_struct env "Point" with
  | Some [ ("x", Int); ("y", Int) ] -> ()
  | _ -> assert_failure "expected struct Point with fields x:int, y:int"
  end;
  (* var_name also declares a variable of the struct type *)
  let env2 = default_env () in
  ignore
    (typecheck_stmt env2
       (StructDef
          {
            pos = dummy_pos;
            tag = "Vec2";
            fields = [ (VFloat, "x"); (VFloat, "y") ];
            var_name = Some "v";
          }));
  begin match lookup_var env2 "v" with
  | Some t -> assert_equal ~printer:string_of_typ (Struct "Vec2") t
  | None ->
      assert_failure
        "expected variable v declared after StructDef with var_name"
  end

let test_member_access _ =
  let env =
    let env = env_with [ ("pt", Struct "Point") ] in
    Hashtbl.add env.structs "Point" [ ("x", Int); ("y", Int) ];
    env
  in
  (* field access gives the declared field type *)
  check_typ ~env Int (MemberAccess (p, !"pt", "x"));
  check_typ ~env Int (MemberAccess (p, !"pt", "y"));
  (* field can be used as an lvalue in assignment *)
  check_typ ~env Int (Assign (p, MemberAccess (p, !"pt", "x"), i 42));
  (* accessing a non-struct type fails *)
  check_err "member access on non-struct type 'int'"
    (MemberAccess (p, i 1, "x"));
  (* accessing a non-existent field fails *)
  check_err ~env "struct 'Point' has no field 'z'"
    (MemberAccess (p, !"pt", "z"))

let test_return_typecheck _ =
  (* return outside a function always fails *)
  check_stmt_err "return statement outside of a function" (ret (i 1));
  check_stmt_err "return statement outside of a function" ret_void;
  (* return void from void function succeeds *)
  let void_env = { (default_env ()) with return_typ = Some Void } in
  begin match typecheck_stmt void_env ret_void with
  | ReturnStmt (_, None) -> ()
  | _ -> assert_failure "expected void return to succeed"
  end;
  (* return expr in void function fails *)
  check_stmt_err ~env:void_env "expected type 'void' but got 'int'" (ret (i 1));
  (* return correct type succeeds *)
  let int_env = { (default_env ()) with return_typ = Some Int } in
  begin match typecheck_stmt int_env (ret (i 0)) with
  | ReturnStmt (_, Some e) ->
      assert_equal ~printer:string_of_typ Int (expr_typ e)
  | _ -> assert_failure "expected int return to succeed"
  end;
  (* return wrong type fails: pointer is not implicitly convertible to int *)
  let ptr_env = { (env_with [ ("p", Ptr Int) ]) with return_typ = Some Int } in
  check_stmt_err ~env:ptr_env "expected type 'int' but got 'int*'" (ret !"p");
  (* return with no value in non-void function fails *)
  check_stmt_err ~env:int_env "expected type 'int' but got 'void'" ret_void

let test_var_scope _ =
  (* a variable defined inside a CompoundStmt is not visible outside *)
  let env = default_env () in
  ignore
    (typecheck_stmt env (CompoundStmt (dummy_pos, [ var_def VInt "inner" ])));
  assert_equal None (lookup_var env "inner")

let tests =
  "typechecker"
  >::: [
         "type_helpers" >:: test_type_helpers;
         "type_error_strings" >:: test_type_error_strings;
         "stmt_fallthrough_helpers" >:: test_stmt_fallthrough_helpers;
         "literals" >:: test_literals;
         "float_minimal" >:: test_float;
         "arithmetic" >:: test_arithmetic;
         "bitwise" >:: test_bitwise;
         "comparison" >:: test_comparison;
         "equality" >:: test_equality;
         "logical" >:: test_logical;
         "unary" >:: test_unary;
         "unary_conversions" >:: test_unary_conversions;
         "nested" >:: test_nested;
         "arithmetic_errors" >:: test_arithmetic_errors;
         "comparison_errors" >:: test_comparison_errors;
         "equality_errors" >:: test_equality_errors;
         "conversions" >:: test_conversions;
         "logical_errors" >:: test_logical_errors;
         "unary_errors" >:: test_unary_errors;
         "var_ref" >:: test_var_ref;
         "var_ref_errors" >:: test_var_ref_errors;
         "assign" >:: test_assign;
         "assign_errors" >:: test_assign_errors;
         "func_call" >:: test_func_call;
         "func_call_errors" >:: test_func_call_errors;
         "ternary" >:: test_ternary;
         "array_decay_exprs" >:: test_array_decay_exprs;
         "array_decay_stmts" >:: test_array_decay_stmts;
         "ternary_errors" >:: test_ternary_errors;
         "var_type_resolution" >:: test_var_type_resolution;
         "typedefs" >:: test_typedefs;
         "typedef_scope" >:: test_typedef_scope;
         "typedef_cycle" >:: test_typedef_cycle;
         "array_param_decay" >:: test_array_param_decay;
         "cast" >:: test_cast;
         "cast_errors" >:: test_cast_errors;
         "int_promotion" >:: test_int_promotion;
         "sign_zero_ext" >:: test_sign_zero_ext;
         "implicit_casts" >:: test_implicit_casts;
         "pointers" >:: test_pointers;
         "null_ptr_constant" >:: test_null_ptr_constant;
         "pointer_errors" >:: test_pointer_errors;
         "pointer_arithmetic" >:: test_pointer_arithmetic;
         "pointer_arithmetic_errors" >:: test_pointer_arithmetic_errors;
         "void_ptr" >:: test_void_ptr;
         "stmt_conditions" >:: test_stmt_conditions;
         "break_continue" >:: test_break_continue;
         "break_continue_errors" >:: test_break_continue_errors;
         "missing_return" >:: test_missing_return;
         "typecheck_program" >:: test_typecheck_program;
         "func_decl" >:: test_func_decl;
         "variadic_func_call" >:: test_variadic_func_call;
         "incdec" >:: test_incdec;
         "incdec_errors" >:: test_incdec_errors;
         "subscript" >:: test_subscript;
         "sizeof" >:: test_sizeof;
         "struct_def" >:: test_struct_def;
         "member_access" >:: test_member_access;
         "return_typecheck" >:: test_return_typecheck;
         "var_scope" >:: test_var_scope;
       ]

let _ = run_test_tt_main tests
