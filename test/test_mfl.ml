open OUnit2
open Mfl

let assert_parse_equal expected input =
  assert_equal ~printer:Fun.id expected (Parser.parse input |> Ast.pp_expr)

let assert_parse_fails err input =
  assert_raises (Parser.Parse_error err) (fun () ->
      Parser.parse input |> Ast.pp_expr)

let assert_interpret_int expected input =
  assert_equal ~printer:Ast.pp_expr (Ast.IntLiteral expected)
    (Interpreter.interpret (Parser.parse input))

let assert_interpret_bool expected input =
  assert_equal ~printer:Ast.pp_expr (Ast.BoolLiteral expected)
    (Interpreter.interpret (Parser.parse input))

let test_parse_literals _ =
  assert_parse_equal "42" "42";
  assert_parse_equal "0" "0";
  assert_parse_equal "true" "true";
  assert_parse_equal "false" "false"

let test_parse_arithmetic _ =
  assert_parse_equal "1 + 2" "1 + 2";
  assert_parse_equal "1 - 2" "1 - 2";
  assert_parse_equal "3 * 4" "3 * 4";
  assert_parse_equal "8 / 2" "8 / 2";
  assert_parse_equal "7 % 3" "7 % 3"

let test_parse_precedence _ =
  (* * binds tighter than + *)
  assert_parse_equal "1 + 2 * 3" "1+2*3";
  assert_parse_equal "1 + 2 * 3" "1 + 2 * 3";
  (* parens override precedence *)
  assert_parse_equal "(1 + 2) * 3" "(1 + 2) * 3";
  (* - and / are left-associative; right-grouped needs parens *)
  assert_parse_equal "8 / (4 / 2)" "8 / (4 / 2)";
  assert_parse_equal "7 - (2 - 1)" "7 - (2 - 1)";
  (* left-associative default *)
  assert_parse_equal "1 + 2 + 3" "1 + 2 + 3";
  assert_parse_equal "1 - 2 - 3" "1 - 2 - 3"

let test_parse_comparison _ =
  assert_parse_equal "1 < 2" "1 < 2";
  assert_parse_equal "1 > 2" "1 > 2";
  assert_parse_equal "1 == 2" "1 == 2";
  assert_parse_equal "1 != 2" "1 != 2";
  assert_parse_equal "1 <= 2" "1 <= 2";
  assert_parse_equal "1 >= 2" "1 >= 2"

let test_parse_boolean_logic _ =
  assert_parse_equal "true && false" "true && false";
  assert_parse_equal "true || false" "true || false";
  (* && binds tighter than || *)
  assert_parse_equal "true || true && false" "true || true && false";
  assert_parse_equal "(true || true) && false" "(true || true) && false"

let test_parse_bitwise _ =
  assert_parse_equal "3 & 5" "3 & 5";
  assert_parse_equal "3 | 5" "3 | 5";
  assert_parse_equal "3 ^ 5" "3 ^ 5";
  (* bitwise precedence: & > ^ > | *)
  assert_parse_equal "1 | 2 ^ 3 & 4" "1 | (2 ^ (3 & 4))";
  (* arithmetic binds tighter than bitwise *)
  assert_parse_equal "1 + 2 & 3" "(1 + 2) & 3"

let test_parse_mixed_precedence _ =
  (* comparison lower than arithmetic *)
  assert_parse_equal "1 + 2 < 3 + 4" "(1 + 2) < (3 + 4)";
  (* && lower than comparison *)
  assert_parse_equal "1 < 2 && 3 > 0" "(1 < 2) && (3 > 0)";
  (* || lower than && *)
  assert_parse_equal "1 == 1 || 2 == 3 && 4 == 4" "1 == 1 || (2 == 3 && 4 == 4)"

let test_parse_errors _ =
  assert_parse_fails "unexpected end of input" "";
  assert_parse_fails "unexpected end of input" "1 +";
  assert_parse_fails "unexpected trailing input" "1 + 2 3";
  assert_parse_fails "expected ')'" "(1 + 2";
  assert_parse_fails "unknown operator '!'" "1 != 2 ! 3"

let test_interpret_arithmetic _ =
  assert_interpret_int 3 "1 + 2";
  assert_interpret_int 5 "8 - 3";
  assert_interpret_int 6 "2 * 3";
  assert_interpret_int 4 "8 / 2";
  assert_interpret_int 1 "7 % 3";
  assert_raises (Failure "div by zero") (fun () ->
      Interpreter.interpret (Parser.parse "1 / 0"));
  assert_raises (Failure "div by zero") (fun () ->
      Interpreter.interpret (Parser.parse "5 % 0"));
  (* precedence is preserved through evaluation *)
  assert_interpret_int 7 "1 + 2 * 3";
  assert_interpret_int 9 "(1 + 2) * 3";
  assert_interpret_int 4 "8 / (4 / 2)";
  assert_interpret_int 6 "7 - (2 - 1)"

let test_interpret_comparison _ =
  assert_interpret_bool true "1 < 2";
  assert_interpret_bool false "2 < 1";
  assert_interpret_bool true "2 > 1";
  assert_interpret_bool false "1 > 2";
  assert_interpret_bool true "1 == 1";
  assert_interpret_bool false "1 == 2";
  assert_interpret_bool true "1 != 2";
  assert_interpret_bool false "1 != 1";
  assert_interpret_bool true "1 <= 1";
  assert_interpret_bool true "1 <= 2";
  assert_interpret_bool false "2 <= 1";
  assert_interpret_bool true "2 >= 2";
  assert_interpret_bool true "3 >= 2";
  assert_interpret_bool false "1 >= 2"

let test_interpret_boolean_logic _ =
  assert_interpret_bool true "true && true";
  assert_interpret_bool false "true && false";
  assert_interpret_bool false "false && true";
  assert_interpret_bool false "false && false";
  assert_interpret_bool true "true || false";
  assert_interpret_bool true "false || true";
  assert_interpret_bool false "false || false";
  assert_interpret_bool true "true || true"

let test_interpret_bitwise _ =
  assert_interpret_int 1 "3 & 5";
  (* 011 & 101 = 001 *)
  assert_interpret_int 7 "3 | 5";
  (* 011 | 101 = 111 *)
  assert_interpret_int 6 "3 ^ 5";
  (* 011 ^ 101 = 110 *)
  assert_interpret_int 0 "7 & 0";
  assert_interpret_int 15 "7 | 8";
  assert_interpret_int 0 "5 ^ 5"

let test_interpret_nested _ =
  assert_interpret_int 14 "2 * 3 + 4 * 2";
  assert_interpret_bool true "2 * 3 == 6";
  assert_interpret_bool true "10 % 3 == 1";
  assert_interpret_bool true "1 < 2 && 2 < 3"

let tests =
  "mfl"
  >::: [
         "parse_literals" >:: test_parse_literals;
         "parse_arithmetic" >:: test_parse_arithmetic;
         "parse_precedence" >:: test_parse_precedence;
         "parse_comparison" >:: test_parse_comparison;
         "parse_boolean_logic" >:: test_parse_boolean_logic;
         "parse_bitwise" >:: test_parse_bitwise;
         "parse_mixed_precedence" >:: test_parse_mixed_precedence;
         "parse_errors" >:: test_parse_errors;
         "interpret_arithmetic" >:: test_interpret_arithmetic;
         "interpret_comparison" >:: test_interpret_comparison;
         "interpret_boolean_logic" >:: test_interpret_boolean_logic;
         "interpret_bitwise" >:: test_interpret_bitwise;
         "interpret_nested" >:: test_interpret_nested;
       ]

let _ = run_test_tt_main tests
