open OUnit2
open Mfl

(* Helpers for parse-based tests *)
let pp_opt_expr = function
  | None -> "(none)"
  | Some e -> Pretty.pp_expr e

let check_int expected input =
  match Interpreter.interpret (Parser.parse input) with
  | Some (Ast.IntLiteral (_, n)) ->
      assert_equal ~printer:string_of_int expected n
  | result ->
      assert_failure
        (Printf.sprintf "expected int %d, got: %s" expected (pp_opt_expr result))

let check_bool expected input =
  match Interpreter.interpret (Parser.parse input) with
  | Some (Ast.BoolLiteral (_, b)) ->
      assert_equal ~printer:string_of_bool expected b
  | result ->
      assert_failure
        (Printf.sprintf "expected bool %b, got: %s" expected
           (pp_opt_expr result))

let check_empty input =
  assert_equal ~printer:pp_opt_expr None
    (Interpreter.interpret (Parser.parse input))

let check_type_error msg input =
  match Parser.parse input |> Interpreter.interpret with
  | _ -> assert_failure (Printf.sprintf "expected Type_error for: %s" input)
  | exception Interpreter.Type_error (_, m) ->
      assert_equal ~printer:Fun.id msg m

let test_literals _ =
  check_empty ";";
  check_empty ";;;;;;;;;;;;;;;;;;;;;;;;";
  check_int 0 "0;";
  check_int 1 "1;";
  check_int 1 "1;;";
  check_int 42 "42;";
  check_bool true "true;;;";
  check_bool true "true;";
  check_bool false "false;"

let test_arithmetic _ =
  (* basic operations *)
  check_int 3 "1 + 2;";
  check_int 5 "8 - 3;";
  check_int 6 "2 * 3;";
  check_int 4 "8 / 2;";
  check_int 1 "7 % 3;";
  (* identity / zero cases *)
  check_int 0 "0 + 0;";
  check_int 0 "5 - 5;";
  check_int 0 "0 * 99;";
  check_int 0 "0 / 5;";
  check_int 0 "6 % 3;";
  (* division rounds toward zero *)
  check_int 3 "7 / 2;";
  (* division by zero *)
  (match Interpreter.interpret (Parser.parse "1 / 0;") with
  | _ -> assert_failure "expected Div_by_zero"
  | exception Interpreter.Div_by_zero _ -> ());
  (match Interpreter.interpret (Parser.parse "5 % 0;") with
  | _ -> assert_failure "expected Div_by_zero"
  | exception Interpreter.Div_by_zero _ -> ());
  (* precedence *)
  check_int 7 "1 + 2 * 3;";
  check_int 9 "(1 + 2) * 3;";
  check_int 4 "8 / (4 / 2);";
  check_int 6 "7 - (2 - 1);";
  (* left-associativity *)
  check_int 2 "1 - 2 + 3;";
  check_int 2 "8 / 2 / 2;"

let test_unary _ =
  (* negation *)
  check_int (-1) "-1;";
  check_int (-42) "-42;";
  check_int 0 "-0;";
  (* double negation *)
  check_int 5 "--5;";
  (* negate a subexpression *)
  check_int (-3) "-(1 + 2);";
  check_int (-6) "-(2 * 3);";
  (* unary in binary context *)
  check_int 1 "-1 + 2;";
  check_int (-5) "-3 + -2;";
  (* logical not *)
  check_bool false "!true;";
  check_bool true "!false;";
  (* double not *)
  check_bool true "!!true;";
  check_bool false "!!false;";
  (* not applied to a comparison result *)
  check_bool true "!(1 == 2);";
  check_bool false "!(1 == 1);";
  check_bool false "!(1 < 2);"

let test_comparison _ =
  (* less-than *)
  check_bool true "1 < 2;";
  check_bool false "2 < 1;";
  check_bool false "1 < 1;";
  (* greater-than *)
  check_bool true "2 > 1;";
  check_bool false "1 > 2;";
  check_bool false "1 > 1;";
  (* equal / not-equal on ints *)
  check_bool true "1 == 1;";
  check_bool false "1 == 2;";
  check_bool true "1 != 2;";
  check_bool false "1 != 1;";
  (* leq / geq *)
  check_bool true "1 <= 1;";
  check_bool true "1 <= 2;";
  check_bool false "2 <= 1;";
  check_bool true "2 >= 2;";
  check_bool true "3 >= 2;";
  check_bool false "1 >= 2;"

let test_boolean_logic _ =
  check_bool true "true && true;";
  check_bool false "true && false;";
  check_bool false "false && true;";
  check_bool false "false && false;";
  check_bool true "true || false;";
  check_bool true "false || true;";
  check_bool false "false || false;";
  check_bool true "true || true;";
  (* && binds tighter than || *)
  check_bool true "true || false && false;";
  check_bool false "(true || false) && false;";
  (* comparison ops *)
  check_bool true "true == true;";
  check_bool true "false == false;";
  check_bool false "false == true;";
  check_bool false "true == false;";
  check_bool false "false != false;";
  check_bool true "false != true;";
  check_bool true "true != false;";
  check_bool false "true != true;";
  (* short circuit *)
  check_bool false "false && (1 / 0 == 0);";
  check_bool true "true || (1 / 0 == 0);";
  (match Interpreter.interpret (Parser.parse "true && (1 / 0 == 0);") with
  | _ -> assert_failure "expected Div_by_zero"
  | exception Interpreter.Div_by_zero _ -> ());
  match Interpreter.interpret (Parser.parse "false || (1 / 0 == 0);") with
  | _ -> assert_failure "expected Div_by_zero"
  | exception Interpreter.Div_by_zero _ -> ()

let test_bitwise _ =
  (* 011 & 101 = 001 *)
  check_int 1 "3 & 5;";
  (* 011 | 101 = 111 *)
  check_int 7 "3 | 5;";
  (* 011 ^ 101 = 110 *)
  check_int 6 "3 ^ 5;";
  (* zero cases *)
  check_int 0 "7 & 0;";
  check_int 5 "5 | 0;";
  check_int 0 "0 ^ 0;";
  (* identity cases *)
  check_int 5 "5 & 5;";
  check_int 15 "7 | 8;";
  check_int 0 "5 ^ 5;";
  (* precedence: & > ^ *)
  check_int 3 "1 | 2 ^ 3 & 4;"

let test_nested _ =
  check_int 14 "2 * 3 + 4 * 2;";
  check_bool true "2 * 3 == 6;";
  check_bool true "10 % 3 == 1;";
  check_bool true "1 < 2 && 2 < 3;";
  (* unary inside binary *)
  check_int 1 "-1 + 2;";
  check_int (-5) "-(2 + 3);";
  check_bool true "!false && true;";
  check_bool false "!(2 > 1);";
  (* deeply nested via parser *)
  check_int 6 "(1 + 2) * (5 - 3);";
  check_bool true "1 + 1 == 2;"

let test_multiple_statements _ =
  check_int 2 "1; 2;";
  check_bool true "false; true;";
  check_int 3 "; 1; ; 3; ;"

let test_compound_statements _ =
  check_empty "{}";
  check_empty "{;;}";
  check_int 2 "{1; 2;}";
  check_bool true "{false; true;}";
  check_int 4 "1; {2; 4;}";
  check_int 3 "{1; {2; 3;};}"

let test_type_errors _ =
  (* arithmetic requires int operands *)
  check_type_error "+: invalid operand types (int, bool)" "1 + false;";
  check_type_error "+: invalid operand types (bool, int)" "true + 1;";
  check_type_error "-: invalid operand types (bool, int)" "false - 1;";
  check_type_error "*: invalid operand types (int, bool)" "2 * true;";
  check_type_error "/: invalid operand types (bool, int)" "true / 2;";
  check_type_error "%: invalid operand types (int, bool)" "5 % false;";
  (* logical ops require bool operands *)
  check_type_error "&&: invalid operand types (int, bool)" "1 && true;";
  check_type_error "&&: invalid operand types (int, int)" "1 && 2;";
  check_type_error "||: invalid operand types (bool, int)" "false || 1;";
  (* comparison ops not defined for bools *)
  check_type_error "<: invalid operand types (bool, bool)" "true < false;";
  check_type_error ">: invalid operand types (bool, bool)" "true > false;";
  check_type_error "<=: invalid operand types (bool, bool)" "true <= false;";
  check_type_error ">=: invalid operand types (bool, bool)" "true >= false;";
  (* bitwise ops not defined for bools *)
  check_type_error "&: invalid operand types (bool, bool)" "true & false;";
  check_type_error "|: invalid operand types (bool, bool)" "true | false;";
  check_type_error "^: invalid operand types (bool, bool)" "true ^ false;";
  (* unary type errors *)
  check_type_error "-: invalid operand type (bool)" "-true;";
  check_type_error "-: invalid operand type (bool)" "-false;";
  check_type_error "!: invalid operand type (int)" "!1;";
  check_type_error "!: invalid operand type (int)" "!0;"

let tests =
  "interpreter"
  >::: [
         "literals" >:: test_literals;
         "arithmetic" >:: test_arithmetic;
         "unary" >:: test_unary;
         "comparison" >:: test_comparison;
         "boolean_logic" >:: test_boolean_logic;
         "bitwise" >:: test_bitwise;
         "nested" >:: test_nested;
         "multiple_statements" >:: test_multiple_statements;
         "compound_statements" >:: test_compound_statements;
         "type_errors" >:: test_type_errors;
       ]

let _ = run_test_tt_main tests
