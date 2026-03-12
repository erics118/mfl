open OUnit2
open Mfl

let check expected input =
  assert_equal ~printer:Fun.id expected (Parser.parse input |> Ast.pp_expr)

let fails err input =
  assert_raises (Parser.Parse_error err) (fun () -> Parser.parse input)

let test_literals _ =
  check "42" "42";
  check "0" "0";
  check "true" "true";
  check "false" "false"

let test_arithmetic _ =
  check "1 + 2" "1 + 2";
  check "1 - 2" "1 - 2";
  check "3 * 4" "3 * 4";
  check "8 / 2" "8 / 2";
  check "7 % 3" "7 % 3"

let test_precedence _ =
  (* * binds tighter than + *)
  check "1 + 2 * 3" "1+2*3";
  check "1 + 2 * 3" "1 + 2 * 3";
  (* parens override precedence *)
  check "(1 + 2) * 3" "(1 + 2) * 3";
  (* - and / are left-associative; right-grouped needs parens *)
  check "8 / (4 / 2)" "8 / (4 / 2)";
  check "7 - (2 - 1)" "7 - (2 - 1)";
  (* left-associative default *)
  check "1 + 2 + 3" "1 + 2 + 3";
  check "1 - 2 - 3" "1 - 2 - 3"

let test_comparison _ =
  check "1 < 2" "1 < 2";
  check "1 > 2" "1 > 2";
  check "1 == 2" "1 == 2";
  check "1 != 2" "1 != 2";
  check "1 <= 2" "1 <= 2";
  check "1 >= 2" "1 >= 2"

let test_boolean_logic _ =
  check "true && false" "true && false";
  check "true || false" "true || false";
  (* && binds tighter than || *)
  check "true || true && false" "true || true && false";
  check "(true || true) && false" "(true || true) && false"

let test_bitwise _ =
  check "3 & 5" "3 & 5";
  check "3 | 5" "3 | 5";
  check "3 ^ 5" "3 ^ 5";
  (* bitwise precedence, highest to lowest: & ^ | *)
  check "1 | 2 ^ 3 & 4" "1 | (2 ^ (3 & 4))";
  (* arithmetic binds tighter than bitwise *)
  check "1 + 2 & 3" "(1 + 2) & 3"

let test_mixed_precedence _ =
  (* comparison lower than arithmetic *)
  check "1 + 2 < 3 + 4" "(1 + 2) < (3 + 4)";
  (* && lower than comparison *)
  check "1 < 2 && 3 > 0" "(1 < 2) && (3 > 0)";
  (* || lower than && *)
  check "1 == 1 || 2 == 3 && 4 == 4" "1 == 1 || (2 == 3 && 4 == 4)"

let test_unary _ =
  check "-1" "-1";
  check "-42" "-42";
  check "-(-5)" "--5";
  check "!true" "!true";
  check "!false" "!false";
  check "!(!true)" "!!true";
  (* unary binds tighter than binary *)
  check "-1 + 2" "-1 + 2";
  check "!true && false" "!true && false";
  (* unary over a binary subexpr requires parens *)
  check "-(1 + 2)" "-(1 + 2)";
  check "!(1 == 2)" "!(1 == 2)"

let test_errors _ =
  fails "unexpected end of input" "";
  fails "unexpected end of input" "1 +";
  fails "unexpected trailing input" "1 + 2 3";
  fails "expected ')'" "(1 + 2";
  fails "unexpected trailing input" "1 != 2 ! 3";
  fails "unknown token: )" ")";
  fails "unknown token: +" "+ 1"

let tests =
  "parser"
  >::: [
         "literals" >:: test_literals;
         "arithmetic" >:: test_arithmetic;
         "precedence" >:: test_precedence;
         "comparison" >:: test_comparison;
         "boolean_logic" >:: test_boolean_logic;
         "bitwise" >:: test_bitwise;
         "mixed_precedence" >:: test_mixed_precedence;
         "unary" >:: test_unary;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
