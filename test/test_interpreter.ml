open OUnit2
open Mfl

let check_int expected input =
  assert_equal ~printer:Ast.pp_expr (Ast.IntLiteral expected)
    (Interpreter.interpret (Parser.parse input))

let check_bool expected input =
  assert_equal ~printer:Ast.pp_expr (Ast.BoolLiteral expected)
    (Interpreter.interpret (Parser.parse input))

let check_type_error msg input =
  assert_raises (Interpreter.Type_error msg) (fun () ->
      Parser.parse input |> Interpreter.interpret)

let test_arithmetic _ =
  check_int 3 "1 + 2";
  check_int 5 "8 - 3";
  check_int 6 "2 * 3";
  check_int 4 "8 / 2";
  check_int 1 "7 % 3";
  assert_raises Interpreter.Div_by_zero (fun () ->
      Interpreter.interpret (Parser.parse "1 / 0"));
  assert_raises Interpreter.Div_by_zero (fun () ->
      Interpreter.interpret (Parser.parse "5 % 0"));
  (* precedence is preserved through evaluation *)
  check_int 7 "1 + 2 * 3";
  check_int 9 "(1 + 2) * 3";
  check_int 4 "8 / (4 / 2)";
  check_int 6 "7 - (2 - 1)"

let test_comparison _ =
  check_bool true "1 < 2";
  check_bool false "2 < 1";
  check_bool true "2 > 1";
  check_bool false "1 > 2";
  check_bool true "1 == 1";
  check_bool false "1 == 2";
  check_bool true "1 != 2";
  check_bool false "1 != 1";
  check_bool true "1 <= 1";
  check_bool true "1 <= 2";
  check_bool false "2 <= 1";
  check_bool true "2 >= 2";
  check_bool true "3 >= 2";
  check_bool false "1 >= 2"

let test_boolean_logic _ =
  check_bool true "true && true";
  check_bool false "true && false";
  check_bool false "false && true";
  check_bool false "false && false";
  check_bool true "true || false";
  check_bool true "false || true";
  check_bool false "false || false";
  check_bool true "true || true"

let test_bitwise _ =
  check_int 1 "3 & 5";
  (* 011 & 101 = 001 *)
  check_int 7 "3 | 5";
  (* 011 | 101 = 111 *)
  check_int 6 "3 ^ 5";
  (* 011 ^ 101 = 110 *)
  check_int 0 "7 & 0";
  check_int 15 "7 | 8";
  check_int 0 "5 ^ 5"

let test_nested _ =
  check_int 14 "2 * 3 + 4 * 2";
  check_bool true "2 * 3 == 6";
  check_bool true "10 % 3 == 1";
  check_bool true "1 < 2 && 2 < 3"

let test_type_errors _ =
  check_type_error "+: invalid operand types (int, bool)" "1 + false";
  check_type_error "-: invalid operand types (bool, int)" "false - 1";
  check_type_error "&&: invalid operand types (int, bool)" "1 && true";
  check_type_error "||: invalid operand types (bool, int)" "false || 1"

let tests =
  "interpreter"
  >::: [
         "arithmetic" >:: test_arithmetic;
         "comparison" >:: test_comparison;
         "boolean_logic" >:: test_boolean_logic;
         "bitwise" >:: test_bitwise;
         "nested" >:: test_nested;
         "type_errors" >:: test_type_errors;
       ]

let _ = run_test_tt_main tests
