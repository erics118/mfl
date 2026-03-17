open OUnit2
open Mfl
open Ast
open Typechecker

let p = Parsed dummy_pos

let empty_env =
  { vars = Hashtbl.create 4; funcs = Hashtbl.create 4; return_typ = None }

let i n = IntLiteral (p, n)
let b b = BoolLiteral (p, b)
let bi op l r = BinaryOp (p, op, l, r)
let un op e = UnaryOp (p, op, e)

let check_typ expected expr =
  let result = typecheck_expr empty_env expr in
  assert_equal ~printer:string_of_typ expected (expr_typ result)

let check_err expected_err expr =
  match typecheck_expr empty_env expr with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, e) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error e)

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

let tern c t e = Ternary (p, c, t, e)

let test_ternary _ =
  check_typ Int (tern (b true) (i 1) (i 2));
  check_typ Bool (tern (b false) (b true) (b false));
  check_typ Bool (tern (bi Less (i 1) (i 2)) (b true) (b false))

let test_ternary_errors _ =
  check_err "condition must be 'bool' but got 'int'" (tern (i 1) (i 2) (i 3));
  check_err "expected type 'int' but got 'bool'" (tern (b true) (i 1) (b false));
  check_err "expected type 'bool' but got 'int'" (tern (b true) (b false) (i 1))

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
         "ternary" >:: test_ternary;
         "ternary_errors" >:: test_ternary_errors;
       ]

let _ = run_test_tt_main tests
