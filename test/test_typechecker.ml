open OUnit2
open Mfl
open Ast
open Typechecker

let p = Parsed dummy_pos

let empty_env =
  {
    vars = [ Hashtbl.create 4 ];
    funcs = Hashtbl.create 4;
    return_typ = None;
    in_loop = false;
  }

let i n = IntLiteral (p, n)
let b b = BoolLiteral (p, b)
let bi op l r = BinaryOp (p, op, l, r)
let un op e = UnaryOp (p, op, e)
let tern cond then_e else_e = Ternary (p, cond, then_e, else_e)
let v x = VarRef (p, x)
let a x e = Assign (p, x, e)
let call f args = FuncCall (p, f, args)

let env_with vars =
  let tbl = Hashtbl.create 4 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) vars;
  { empty_env with vars = [ tbl ] }

let env_with_funcs funcs =
  let tbl = Hashtbl.create 4 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) funcs;
  { empty_env with funcs = tbl }

let check_typ ?(env = empty_env) expected e =
  let result = typecheck_expr env e in
  assert_equal ~printer:string_of_typ expected (expr_typ result)

let check_err ?(env = empty_env) expected_err e =
  match typecheck_expr env e with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let check_stmt_err ?(env = empty_env) expected_err s =
  match typecheck_stmt env s with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let test_literals _ =
  check_typ Int (i 0);
  check_typ Int (i 123);
  check_typ Int (i (-1));
  check_typ Bool (b true);
  check_typ Bool (b false)

let test_arithmetic _ =
  check_typ Int (bi Add (i 1) (i 2));
  check_typ Int (bi Sub (i 5) (i 3));
  check_typ Int (bi Mul (i 3) (i 4));
  check_typ Int (bi Div (i 8) (i 2));
  check_typ Int (bi Mod (i 7) (i 3))

let test_bitwise _ =
  check_typ Int (bi BitAnd (i 3) (i 5));
  check_typ Int (bi BitOr (i 3) (i 5));
  check_typ Int (bi BitXor (i 3) (i 5));
  check_typ Int (bi LShift (i 1) (i 2));
  check_typ Int (bi RShift (i 8) (i 1))

let test_comparison _ =
  check_typ Bool (bi Less (i 1) (i 2));
  check_typ Bool (bi Leq (i 1) (i 2));
  check_typ Bool (bi Greater (i 2) (i 1));
  check_typ Bool (bi Geq (i 2) (i 1))

let test_equality _ =
  check_typ Bool (bi Equal (i 1) (i 1));
  check_typ Bool (bi Neq (i 1) (i 2));
  check_typ Bool (bi Equal (b true) (b false));
  check_typ Bool (bi Neq (b true) (b true))

let test_logical _ =
  check_typ Bool (bi And (b true) (b false));
  check_typ Bool (bi Or (b false) (b true))

let test_unary _ =
  check_typ Bool (un Not (b true));
  check_typ Bool (un Not (b false));
  check_typ Int (un Neg (i 1));
  check_typ Int (un Compl (i 0))

let test_nested _ =
  (* (1 + 2) * 3 *)
  check_typ Int (bi Mul (bi Add (i 1) (i 2)) (i 3));
  (* !true && false *)
  check_typ Bool (bi And (un Not (b true)) (b false));
  (* 1 < 2 == true *)
  check_typ Bool (bi Equal (bi Less (i 1) (i 2)) (b true))

let test_arithmetic_errors _ =
  check_err "operator '+': type mismatch between 'int' and 'bool'"
    (bi Add (i 1) (b true));
  check_err "operator '+': type mismatch between 'bool' and 'int'"
    (bi Add (b true) (i 1));
  check_err "operator '*': type mismatch between 'bool' and 'bool'"
    (bi Mul (b true) (b false))

let test_comparison_errors _ =
  check_err "operator '<': type mismatch between 'bool' and 'bool'"
    (bi Less (b true) (b false));
  check_err "operator '>=': type mismatch between 'int' and 'bool'"
    (bi Geq (i 1) (b true))

let test_equality_errors _ =
  check_err "operator '==': type mismatch between 'int' and 'bool'"
    (bi Equal (i 1) (b true));
  check_err "operator '!=': type mismatch between 'bool' and 'int'"
    (bi Neq (b false) (i 0))

let test_logical_errors _ =
  check_err "operator '&&': type mismatch between 'int' and 'int'"
    (bi And (i 1) (i 0));
  check_err "operator '||': type mismatch between 'bool' and 'int'"
    (bi Or (b true) (i 1))

let test_unary_errors _ =
  check_err "operator '!': invalid operand type 'int'" (un Not (i 1));
  check_err "operator '-': invalid operand type 'bool'" (un Neg (b true));
  check_err "operator '~': invalid operand type 'bool'" (un Compl (b false))

let test_var_ref _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int (v "x");
  check_typ ~env Bool (v "ok")

let test_var_ref_errors _ =
  check_err "unbound variable 'x'" (v "x");
  check_err "unbound variable 'y'" (v "y")

let test_assign _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int (a "x" (i 42));
  check_typ ~env Bool (a "ok" (b true));
  check_typ ~env Int (a "x" (bi Add (i 1) (i 2)))

let test_assign_errors _ =
  let env = env_with [ ("x", Int) ] in
  check_err ~env "unbound variable 'y'" (a "y" (i 1));
  check_err ~env "expected type 'int' but got 'bool'" (a "x" (b true))

let test_func_call _ =
  let env =
    env_with_funcs
      [
        ("add", { params = [ Int; Int ]; ret = Int });
        ("ready", { params = []; ret = Bool });
      ]
  in
  check_typ ~env Int (call "add" [ i 1; i 2 ]);
  check_typ ~env Bool (call "ready" [])

let test_func_call_errors _ =
  let env = env_with_funcs [ ("add", { params = [ Int; Int ]; ret = Int }) ] in
  check_err ~env "unbound function 'f'" (call "f" []);
  check_err ~env "'add' expects 2 argument(s) but got 1" (call "add" [ i 1 ]);
  check_err ~env "'add' expects 2 argument(s) but got 0" (call "add" []);
  check_err ~env "expected type 'int' but got 'bool'"
    (call "add" [ b true; i 1 ])

let test_ternary _ =
  check_typ Int (tern (b true) (i 1) (i 2));
  check_typ Bool (tern (b false) (b true) (b false));
  check_typ Bool (tern (bi Less (i 1) (i 2)) (b true) (b false))

let test_ternary_errors _ =
  check_err "condition must be 'bool' but got 'int'" (tern (i 1) (i 2) (i 3));
  check_err "expected type 'int' but got 'bool'" (tern (b true) (i 1) (b false));
  check_err "expected type 'bool' but got 'int'" (tern (b true) (b false) (i 1))

let cast ty e = Cast (p, ty, e)
let pre_inc e = PreInc (p, e)
let post_inc e = PostInc (p, e)
let pre_dec e = PreDec (p, e)
let post_dec e = PostDec (p, e)

let test_incdec _ =
  let env = env_with [ ("x", Int) ] in
  check_typ ~env Int (pre_inc (v "x"));
  check_typ ~env Int (post_inc (v "x"));
  check_typ ~env Int (pre_dec (v "x"));
  check_typ ~env Int (post_dec (v "x"))

let test_incdec_errors _ =
  let env_int = env_with [ ("x", Int) ] in
  let env_bool = env_with [ ("flag", Bool) ] in
  (* non-lvalue operand *)
  check_err ~env:env_int "expression is not an lvalue" (pre_inc (i 1));
  check_err ~env:env_int "expression is not an lvalue"
    (post_inc (bi Add (v "x") (i 1)));
  (* bool operand *)
  check_err ~env:env_bool "operator 'prefix ++': invalid operand type 'bool'"
    (pre_inc (v "flag"));
  check_err ~env:env_bool "operator 'prefix --': invalid operand type 'bool'"
    (pre_dec (v "flag"));
  check_err ~env:env_bool "operator 'postfix ++': invalid operand type 'bool'"
    (post_inc (v "flag"));
  check_err ~env:env_bool "operator 'postfix --': invalid operand type 'bool'"
    (post_dec (v "flag"))

let test_cast _ =
  let env = env_with [ ("x", Long); ("n", Int); ("c", Char) ] in
  check_typ ~env Int (cast VInt (v "x"));
  check_typ ~env Long (cast VLong (v "n"));
  check_typ ~env Bool (cast VBool (v "n"));
  check_typ ~env Char (cast VChar (v "n"));
  check_typ Int (cast VInt (i 5));
  check_typ Long (cast VLong (i 5))

let test_cast_errors _ =
  let env = env_with [ ("x", Long) ] in
  check_err ~env "cannot cast from 'long' to 'void'" (cast VVoid (v "x"))

let test_break_continue _ =
  (* break and continue are valid inside loops *)
  let loop_env = { empty_env with in_loop = true } in
  (match typecheck_stmt loop_env (BreakStmt dummy_pos) with
  | BreakStmt _ -> ()
  | _ -> assert_failure "expected BreakStmt");
  match typecheck_stmt loop_env (ContinueStmt dummy_pos) with
  | ContinueStmt _ -> ()
  | _ -> assert_failure "expected ContinueStmt"

let test_var_type_resolution _ =
  let check vt =
    ignore
      (typecheck_stmt empty_env
         (VarDef { pos = dummy_pos; var_type = vt; name = "x"; init = None }))
  in
  check VBool;
  check VChar;
  check VUChar;
  check VShort;
  check VUShort;
  check VInt;
  check VUInt;
  check VLong;
  check VULong;
  check VLongLong;
  check VULongLong;
  (* VNamed with an unknown name must raise UnknownType *)
  check_stmt_err "unknown type 'Foo'"
    (VarDef
       { pos = dummy_pos; var_type = VNamed "Foo"; name = "x"; init = None })

let test_break_continue_errors _ =
  (* break and continue outside a loop are errors *)
  check_stmt_err "break statement outside of a loop" (BreakStmt dummy_pos);
  check_stmt_err "continue statement outside of a loop" (ContinueStmt dummy_pos)

let tests =
  "typechecker"
  >::: [
         "literals" >:: test_literals;
         "arithmetic" >:: test_arithmetic;
         "bitwise" >:: test_bitwise;
         "comparison" >:: test_comparison;
         "equality" >:: test_equality;
         "logical" >:: test_logical;
         "unary" >:: test_unary;
         "nested" >:: test_nested;
         "arithmetic_errors" >:: test_arithmetic_errors;
         "comparison_errors" >:: test_comparison_errors;
         "equality_errors" >:: test_equality_errors;
         "logical_errors" >:: test_logical_errors;
         "unary_errors" >:: test_unary_errors;
         "var_ref" >:: test_var_ref;
         "var_ref_errors" >:: test_var_ref_errors;
         "assign" >:: test_assign;
         "assign_errors" >:: test_assign_errors;
         "func_call" >:: test_func_call;
         "func_call_errors" >:: test_func_call_errors;
         "ternary" >:: test_ternary;
         "ternary_errors" >:: test_ternary_errors;
         "var_type_resolution" >:: test_var_type_resolution;
         "cast" >:: test_cast;
         "cast_errors" >:: test_cast_errors;
         "break_continue" >:: test_break_continue;
         "break_continue_errors" >:: test_break_continue_errors;
         "incdec" >:: test_incdec;
         "incdec_errors" >:: test_incdec_errors;
       ]

let _ = run_test_tt_main tests
