open OUnit2
open Mfl

let check expected input =
  assert_equal ~printer:Fun.id expected (Parser.parse input |> Pretty.pp_stmt)

let roundtrip expected = check expected expected

let fails err input =
  assert_raises (Parser.Parse_error err) (fun () -> Parser.parse input)

let test_literals _ =
  roundtrip ";";
  roundtrip ";\n;\n;\n;\n;\n;\n;";
  roundtrip "42;";
  roundtrip "42;\n;";
  roundtrip "1;\n2;";
  roundtrip ";\n;\n1;";
  roundtrip "0;";
  roundtrip "true;";
  roundtrip "false;"

let test_arithmetic _ =
  roundtrip "1 + 2;";
  roundtrip "1 - 2;";
  roundtrip "3 * 4;";
  roundtrip "8 / 2;";
  roundtrip "7 % 3;"

let test_precedence _ =
  (* * binds tighter than + *)
  check "1;" "(((1)));";
  check "1 + 2 * 3;" "1 + (2 * 3);";
  roundtrip "1 + 2 * 3;";
  (* parens override precedence *)
  roundtrip "(1 + 2) * 3;";
  (* - and / are left-associative; right-grouped needs parens *)
  roundtrip "8 / (4 / 2);";
  roundtrip "7 - (2 - 1);";
  (* left-associative default *)
  check "1 + 2 + 3;" "(1 + 2) + 3;";
  check "1 - 2 - 3;" "(1 - 2) - 3;";
  check "1 * 2 * 3;" "(1 * 2) * 3;";
  check "1 / 2 / 3;" "(1 / 2) / 3;"

let test_comparison _ =
  roundtrip "1 < 2;";
  roundtrip "1 > 2;";
  roundtrip "1 == 2;";
  roundtrip "1 != 2;";
  roundtrip "1 <= 2;";
  roundtrip "1 >= 2;"

let test_boolean_logic _ =
  roundtrip "true && false;";
  roundtrip "true || false;";
  (* && binds tighter than || *)
  roundtrip "true || true && false;";
  roundtrip "(true || true) && false;";
  check "true && true && false;" "(true && true) && false;";
  check "true || true || false;" "(true || true) || false;"

let test_bitwise _ =
  roundtrip "3 & 5;";
  roundtrip "3 | 5;";
  roundtrip "3 ^ 5;";
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
  roundtrip "-1;";
  roundtrip "-42;";
  check "-(-5);" "--5;";
  roundtrip "!true;";
  roundtrip "!false;";
  check "!(!true);" "!!true;";
  (* unary binds tighter than binary *)
  roundtrip "-1 + 2;";
  roundtrip "!true && false;";
  (* unary over a binary subexpr requires parens *)
  roundtrip "-(1 + 2);";
  roundtrip "!(1 == 2);"

let test_multiple_statements _ =
  roundtrip "1;\n2;\n3;\n4;\n5;";
  roundtrip "1 + 2;\n3 * 4;";
  roundtrip "a;";
  roundtrip "a + 1;";
  roundtrip "a + b * c;";
  roundtrip "a == b;"

let test_var_defs _ =
  roundtrip "int x = 3;";
  roundtrip "int x = 1 + 2 * 3;";
  roundtrip "bool ready = true;";
  roundtrip "bool ok = 1 < 2;";
  roundtrip "CustomType value = 3;";
  roundtrip "int x = 1;\nint y = 2;";
  roundtrip "bool t = true;\nint x = 2;";
  roundtrip "{\n    int x = 3;\n}";
  roundtrip "{\n    bool x = false;\n}";
  roundtrip "{\n    UserType x = 7;\n}";
  roundtrip "1;\nint x = 2;\n3;"

let test_returns _ =
  roundtrip "return;";
  roundtrip "return 1;";
  roundtrip "return 1 + 2 * 3;";
  roundtrip "{\n    return true;\n}";
  roundtrip "int add(int a, int b) {\n    return a + b;\n}";
  roundtrip "bool is_ok() {\n    return true;\n}";
  roundtrip "int add(int a, int b) {\n    int x = a + b;\n    return x;\n}";
  roundtrip "void say_hi() {}";
  roundtrip "void print_it(int x) {\n    printint(x);\n}"

let test_function_calls _ =
  roundtrip "foo();";
  roundtrip "foo(1);";
  roundtrip "foo(1, 2 + 3);";
  roundtrip "foo(bar(1), baz(2 + 3));";
  roundtrip "int x = add(1, 2);";
  roundtrip "return ready();";
  roundtrip "int run() {\n    return add(1, 2 + 3);\n}"

let test_ternary _ =
  roundtrip "a ? b : c;";
  roundtrip "a + 1 ? b : c * d;";
  check "a ? b : c ? d : e;" "a ? b : (c ? d : e);";
  roundtrip "(a ? b : c) ? d : e;";
  roundtrip "return a ? b : c;";
  roundtrip "int pick(int a, int b, int c) {\n    return a ? b : c;\n}";
  roundtrip "1 + (a ? b : c);"

let test_compound_statements _ =
  roundtrip "{}";
  roundtrip "{\n    1 + 2;\n}";
  roundtrip "{\n    1 + 2;\n    3 * 4;\n}";
  roundtrip "1;\n{\n    2;\n    3;\n}\n4;";
  roundtrip "{\n    ;\n    ;\n}"

let test_if _ =
  roundtrip "if (true) {}";
  roundtrip "if (true) {\n    1;\n}";
  roundtrip "if (true) {\n    1;\n} else {\n    2;\n}";
  roundtrip "if (true) {\n    1;\n} else if (false) {\n    2;\n}";
  roundtrip
    "if (true) {\n    1;\n} else if (false) {\n    2;\n} else {\n    3;\n}"

let test_while _ =
  roundtrip "while (true) {}";
  roundtrip "while (i < 10) {\n    i + 1;\n}";
  roundtrip "while (true) {\n    if (x) {\n        return;\n    }\n}"

let test_assign _ =
  roundtrip "x = 1;";
  roundtrip "x = 1 + 2;";
  roundtrip "{\n    x = 0;\n    x = x + 1;\n}"

let test_for _ =
  roundtrip "for (int i = 0; i < 10; i + 1) {\n    i;\n}";
  roundtrip "for (int i = 0; i < n; i + 1) {}";
  roundtrip "for (x = 0; x < 10; x + 1) {\n    x;\n}"

let test_errors _ =
  fails "unexpected end of input" "";
  fails "unexpected end of input" "1 +";
  fails "expected identifier" "int = 3;";
  fails "expected '='" "int x 3;";
  fails "unknown token: ';'" "int x = ;";
  fails "expected ';'" "int x = 3";
  fails "expected identifier" "bool = true;";
  fails "expected '='" "bool x true;";
  (* fails "expected ';'" "UserType = 1;"; *)
  fails "expected '='" "UserType x 1;";
  fails "expected ';'" "return 1";
  fails "unexpected end of input" "return";
  fails "expected ':'" "a ? b;";
  fails "unexpected end of input" "a ? b :";
  fails "unknown token: ';'" "a ? ; : b;";
  fails "expected ',' or ')'" "foo(1;";
  fails "unknown token: ')'" "foo(1,);";
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
         "function_calls" >:: test_function_calls;
         "ternary" >:: test_ternary;
         "compound_statements" >:: test_compound_statements;
         "if" >:: test_if;
         "assign" >:: test_assign;
         "while" >:: test_while;
         "for" >:: test_for;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
