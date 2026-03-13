open OUnit2
open Mfl

let check expected input =
  assert_equal ~printer:Fun.id expected (Parser.parse input |> Ast.pp_expr)

let fails err input =
  assert_raises (Parser.Parse_error err) (fun () -> Parser.parse input)

let test_literals _ =
  check ";" ";";
  check ";\n;\n;\n;\n;\n;\n;" ";;;;;;;";
  check "42;" "42;";
  check "42;\n;" "42;;";
  check "1;\n2;" "1; 2;";
  check ";\n;\n1;" ";; 1;";
  check "0;" "0;";
  check "true;" "true;";
  check "false;" "false;"

let test_arithmetic _ =
  check "1 + 2;" "1 + 2;";
  check "1 - 2;" "1 - 2;";
  check "3 * 4;" "3 * 4;";
  check "8 / 2;" "8 / 2;";
  check "7 % 3;" "7 % 3;"

let test_precedence _ =
  (* * binds tighter than + *)
  check "1 + 2 * 3;" "1+2*3;";
  check "1 + 2 * 3;" "1 + 2 * 3;";
  (* parens override precedence *)
  check "(1 + 2) * 3;" "(1 + 2) * 3;";
  (* - and / are left-associative; right-grouped needs parens *)
  check "8 / (4 / 2);" "8 / (4 / 2);";
  check "7 - (2 - 1);" "7 - (2 - 1);";
  (* left-associative default *)
  check "1 + 2 + 3;" "1 + 2 + 3;";
  check "1 - 2 - 3;" "1 - 2 - 3;"

let test_comparison _ =
  check "1 < 2;" "1 < 2;";
  check "1 > 2;" "1 > 2;";
  check "1 == 2;" "1 == 2;";
  check "1 != 2;" "1 != 2;";
  check "1 <= 2;" "1 <= 2;";
  check "1 >= 2;" "1 >= 2;"

let test_boolean_logic _ =
  check "true && false;" "true && false;";
  check "true || false;" "true || false;";
  (* && binds tighter than || *)
  check "true || true && false;" "true || true && false;";
  check "(true || true) && false;" "(true || true) && false;"

let test_bitwise _ =
  check "3 & 5;" "3 & 5;";
  check "3 | 5;" "3 | 5;";
  check "3 ^ 5;" "3 ^ 5;";
  (* bitwise precedence, highest to lowest: & ^ | *)
  check "1 | 2 ^ 3 & 4;" "1 | (2 ^ (3 & 4));";
  (* arithmetic binds tighter than bitwise *)
  check "1 + 2 & 3;" "(1 + 2) & 3;"

let test_mixed_precedence _ =
  (* comparison lower than arithmetic *)
  check "1 + 2 < 3 + 4;" "(1 + 2) < (3 + 4);";
  (* && lower than comparison *)
  check "1 < 2 && 3 > 0;" "(1 < 2) && (3 > 0);";
  (* || lower than && *)
  check "1 == 1 || 2 == 3 && 4 == 4;" "1 == 1 || (2 == 3 && 4 == 4);"

let test_unary _ =
  check "-1;" "-1;";
  check "-42;" "-42;";
  check "-(-5);" "--5;";
  check "!true;" "!true;";
  check "!false;" "!false;";
  check "!(!true);" "!!true;";
  (* unary binds tighter than binary *)
  check "-1 + 2;" "-1 + 2;";
  check "!true && false;" "!true && false;";
  (* unary over a binary subexpr requires parens *)
  check "-(1 + 2);" "-(1 + 2);";
  check "!(1 == 2);" "!(1 == 2);"

let test_multiple_statements _ =
  check "1;\n2;\n3;\n4;\n5;" "1; 2; 3; 4; 5;";
  check "1 + 2;\n3 * 4;" "1 + 2; 3 * 4;";
  check "a;" "a;";
  check "a + 1;" "a + 1;";
  check "a + b * c;" "a + b * c;";
  check "a == b;" "a == b;"

let test_var_defs _ =
  check "int x = 3;" "int x = 3;";
  check "int x = 1 + 2 * 3;" "int x = 1 + 2 * 3;";
  check "bool ready = true;" "bool ready = true;";
  check "bool ok = 1 < 2;" "bool ok = 1 < 2;";
  check "CustomType value = 3;" "CustomType value = 3;";
  check "int x = 1;\nint y = 2;" "int x = 1; int y = 2;";
  check "bool t = true;\nint x = 2;" "bool t = true; int x = 2;";
  check "{int x = 3;}" "{int x = 3;}";
  check "{bool x = false;}" "{bool x = false;}";
  check "{UserType x = 7;}" "{UserType x = 7;}";
  check "1;\nint x = 2;\n3;" "1; int x = 2; 3;"

let test_returns _ =
  check "return;" "return;";
  check "return 1;" "return 1;";
  check "return 1 + 2 * 3;" "return 1 + 2 * 3;";
  check "{return true;}" "{return true;}";
  check "int add(int a, int b) {return a + b;}"
    "int add(int a, int b) { return a + b; }";
  check "bool is_ok() {return true;}" "bool is_ok() { return true; }";
  check "int add(int a, int b) {int x = a + b;\nreturn x;}"
    "int add(int a, int b) { int x = a + b; return x; }"

let test_compound_statements _ =
  check "{}" "{}";
  check "{1 + 2;}" "{1 + 2;}";
  check "{1 + 2;\n3 * 4;}" "{1 + 2; 3 * 4;}";
  check "1;\n{2;\n3;}\n4;" "1; {2; 3;} 4;";
  check "{;\n;}" "{;;}"

let test_errors _ =
  fails "unexpected end of input" "";
  fails "unexpected end of input" "1 +";
  fails "expected identifier" "int = 3;";
  fails "expected '='" "int x 3;";
  fails "unknown token: ';'" "int x = ;";
  fails "expected ';'" "int x = 3";
  fails "expected identifier" "bool = true;";
  fails "expected '='" "bool x true;";
  fails "expected ';'" "UserType = 1;";
  fails "expected '='" "UserType x 1;";
  fails "expected ';'" "return 1";
  fails "unexpected end of input" "return";
  fails "expected '}'" "{";
  fails "expected ';'" "{1";
  fails "expected '}'" "{1;";
  fails "unknown token: ';'" "1 + ;";
  fails "expected ';'" "1 + 2 3";
  fails "expected ';'" "1; 2";
  fails "expected ')'" "(1 + 2";
  fails "expected ';'" "1 != 2 ! 3";
  fails "unknown token: ')'" ")";
  fails "unknown token: '+'" "+ ;"

let tests =
  "parser;"
  >::: [
         "literals" >:: test_literals;
         "arithmetic" >:: test_arithmetic;
         "precedence" >:: test_precedence;
         "comparison" >:: test_comparison;
         "boolean_logic" >:: test_boolean_logic;
         "bitwise" >:: test_bitwise;
         "mixed_precedence" >:: test_mixed_precedence;
         "unary" >:: test_unary;
         "multiple_statements" >:: test_multiple_statements;
         "var_defs" >:: test_var_defs;
         "returns" >:: test_returns;
         "compound_statements" >:: test_compound_statements;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
