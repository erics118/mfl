open OUnit2
open Mfl

let make_state input =
  let st = Parser.create (Lexer.create input) in
  Parser.advance st;
  st

let check expected input =
  assert_equal ~printer:Fun.id expected (Parser.parse input |> Pretty.pp_stmt)

let roundtrip expected = check expected expected

let fails err input =
  match Parser.parse input with
  | _ -> assert_failure (Printf.sprintf "expected Parse_error for: %s" input)
  | exception Parser.Parse_error (_, m) -> assert_equal ~printer:Fun.id err m

let assert_top_level_pos expected_line expected_col input =
  match Parser.parse input with
  | Ast.CompoundStmt ({ line; col }, _) ->
      assert_equal ~printer:string_of_int expected_line line;
      assert_equal ~printer:string_of_int expected_col col
  | _ -> assert_failure "expected top-level CompoundStmt"

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

let test_top_level_pos _ =
  assert_top_level_pos 1 1 "1;";
  assert_top_level_pos 2 3 "\n  1;";
  assert_top_level_pos 2 1 "// hi\n1;";
  assert_top_level_pos 3 2 "/* x */\n\n 1;"

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
  roundtrip "3 << 1;";
  roundtrip "3 >> 1;";
  (* bitwise precedence, highest to lowest: & ^ | *)
  check "1 | 2 ^ 3 & 4;" "1 | (2 ^ (3 & 4));";
  (* arithmetic binds tighter than bitwise *)
  check "1 + 2 & 3;" "(1 + 2) & 3;";
  (* arithmetic binds tighter than shift *)
  check "1 << 2 + 3;" "1 << (2 + 3);";
  check "1 + 2 << 3;" "(1 + 2) << 3;";
  (* shift binds tighter than bitwise & *)
  check "1 & 2 << 3;" "1 & (2 << 3);";
  (* shift is left-associative *)
  check "1 << 2 << 3;" "(1 << 2) << 3;"

let test_mixed_precedence _ =
  (* comparison lower than arithmetic *)
  check "1 + 2 < 3 + 4;" "(1 + 2) < (3 + 4);";
  (* && lower than comparison *)
  check "1 < 2 && 3 > 0;" "(1 < 2) && (3 > 0);";
  (* || lower than && *)
  check "1 == 1 || 2 == 3 && 4 == 4;" "1 == 1 || (2 == 3 && 4 == 4);";
  (* bit ops have lower precedence than comparison *)
  check "a & b == c;" "a & (b == c);";
  check "a | b == c;" "a | (b == c);";
  check "a ^ b < c;" "a ^ (b < c);";
  check "a & b < c;" "a & (b < c);";
  (* equality has lower precedence than relations *)
  check "a < b == c < d;" "(a < b) == (c < d);";
  check "a >= b != c >= d;" "(a >= b) != (c >= d);";
  (* ternary has lower precedence than || *)
  check "a || b ? c : d;" "(a || b) ? c : d;"

let test_unary _ =
  roundtrip "-1;";
  roundtrip "-42;";
  roundtrip "-(-5);";
  roundtrip "&x;";
  roundtrip "*p;";
  (* *& and &* are unambiguous *)
  roundtrip "*&p;";
  roundtrip "&*p;";
  roundtrip "!true;";
  roundtrip "!false;";
  (* !! is unambiguous *)
  roundtrip "!!true;";
  (* ~~ is unambiguous *)
  roundtrip "~~x;";
  roundtrip "-(-x);";
  (* AddrOf of AddrOf would form && without parens *)
  roundtrip "&(&x);";
  (* unary of ternary or assign needs parens *)
  roundtrip "-(a ? b : c);";
  roundtrip "~(x = 5);";
  (* unary binds tighter than binary *)
  roundtrip "-1 + 2;";
  roundtrip "!true && false;";
  roundtrip "~x + 1;";
  roundtrip "~x << 1;";
  (* unary over a binary subexpr requires parens *)
  roundtrip "-(1 + 2);";
  roundtrip "!(1 == 2);";
  roundtrip "~(1 + 2);"

let test_incdec _ =
  roundtrip "--x;";
  roundtrip "x--;";
  roundtrip "++x;";
  roundtrip "x++;";
  (* postfix binds tighter than prefix *)
  check "++x++;" "++(x++);";
  roundtrip "++x++;";
  roundtrip "(++x)++;";
  roundtrip "x++++;";
  roundtrip "++++x;";
  (* postfix binds tighter than (unary) deref *)
  check "*p++;" "*(p++);";
  roundtrip "*p++;";
  roundtrip "*p--;";
  roundtrip "(*p)++;";
  roundtrip "(*p)--;";
  (* postfix binds tighter than neg *)
  check "-x++;" "-(x++);";
  roundtrip "-x++;";
  roundtrip "--x++;";
  (* postfix binds looser than (unary) deref *)
  check "++*p;" "++(*p);";
  check "--*p;" "--(*p);";
  roundtrip "++*p;";
  roundtrip "--*p;"

let test_multiple_statements _ =
  roundtrip "1;\n2;\n3;\n4;\n5;";
  roundtrip "1 + 2;\n3 * 4;";
  roundtrip "a;";
  roundtrip "a + 1;";
  roundtrip "a + b * c;";
  roundtrip "a == b;"

let test_var_defs _ =
  roundtrip "int x;";
  roundtrip "int x = 3;";
  roundtrip "int* p;";
  roundtrip "int* p = &x;";
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

let test_int_types _ =
  roundtrip "char x;";
  roundtrip "short x;";
  roundtrip "int x;";
  roundtrip "long x;";
  roundtrip "long long x;";
  (* unsigned integer types *)
  roundtrip "unsigned char x;";
  roundtrip "unsigned short x;";
  roundtrip "unsigned int x;";
  roundtrip "unsigned long x;";
  roundtrip "unsigned long long x;";
  (* "int" suffix is consumed and removed *)
  check "short x;" "short int x;";
  check "long x;" "long int x;";
  check "long long x;" "long long int x;";
  check "unsigned short x;" "unsigned short int x;";
  check "unsigned long x;" "unsigned long int x;";
  check "unsigned long long x;" "unsigned long long int x;";
  (* signed char is distinct from plain char *)
  check "signed char x;" "signed char x;";
  check "short x;" "signed short x;";
  check "int x;" "signed int x;";
  check "long x;" "signed long x;";
  check "long long x;" "signed long long x;";
  (* signed alone gets converted to int *)
  check "int x;" "signed x;";
  (* unsigned alone gets converted to unsigned int *)
  check "unsigned int x;" "unsigned x;"

let test_char_literal _ =
  roundtrip "'a';";
  roundtrip "char x = 'a';";
  roundtrip "int x = '\\n';";
  roundtrip "'\\\\';";
  roundtrip "'\\t';";
  roundtrip "'\\0';";
  check "'A';" "'\\x41';";
  check "'\\xff';" "'\\xff';";
  roundtrip "signed char x = 'a';";
  roundtrip "unsigned char x = 'a';";
  roundtrip "int x = 'a' + 1;"

let test_pointer_types _ =
  roundtrip "int* p;";
  roundtrip "int** pp;";
  roundtrip "int***** pp;";
  roundtrip "bool* p;";
  check "UserType * p;" "UserType* p;"

let test_array _ =
  roundtrip "int a[10];";
  roundtrip "char a[5];";
  roundtrip "bool a[2];";
  roundtrip "long a[100];";
  (* array of ptr *)
  roundtrip "int* a[10];";
  (* subscript *)
  roundtrip "a[0];";
  roundtrip "a[i];";
  roundtrip "a[i + 1];";
  roundtrip "a[i + 1] = 3;";
  (* 2d array *)
  roundtrip "a[0][1];";
  (* error cases *)
  fails "array size must be a constant integer" "int a[];";
  fails "array size must be a constant integer" "int a[x];"

let test_break_continue _ =
  roundtrip "break;";
  roundtrip "continue;";
  roundtrip "while (true) {\n    break;\n}";
  roundtrip "while (true) {\n    continue;\n}";
  roundtrip "for (;;) {\n    break;\n}";
  roundtrip
    "for (;;) {\n\
    \    if (true) {\n\
    \        break;\n\
    \    } else {\n\
    \        continue;\n\
    \    }\n\
     }"

let test_returns _ =
  roundtrip "return;";
  roundtrip "return 1;";
  roundtrip "int* addr(int* p) {\n    return p;\n}";
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
  roundtrip "foo(&x);";
  roundtrip "foo(*p);";
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
    "if (true) {\n    1;\n} else if (false) {\n    2;\n} else {\n    3;\n}";
  check "if (true) {\n    return;\n}" "if (true) return;";
  check "if (true) {\n    return;\n} else {\n    return;\n}"
    "if (true) return; else return;"

let test_while _ =
  roundtrip "while (true) {}";
  roundtrip "while (i < 10) {\n    i + 1;\n}";
  roundtrip "while (true) {\n    if (x) {\n        return;\n    }\n}";
  check "while (true) {\n    return;\n}" "while (true) return;"

let test_do_while _ =
  roundtrip "do {} while (true);";
  roundtrip "do {\n    i + 1;\n} while (i < 10);";
  roundtrip "do {\n    if (x) {\n        return;\n    }\n} while (true);";
  check "do {\n    return;\n} while (true);" "do return; while (true);"

let test_assign _ =
  roundtrip "x = 1;";
  roundtrip "x = 1 + 2;";
  roundtrip "{\n    x = 0;\n    x = x + 1;\n}";
  (* assign a pointer *)
  roundtrip "*p = 1;";
  roundtrip "*p = *q;";
  (* right associativity *)
  roundtrip "a = b = 5;";
  check "a = b = 5;" "a = (b = 5);";
  (* ptr arith and assign *)
  roundtrip "*(p + 1) = 5;"

let test_for _ =
  roundtrip "for (int i = 0; i < 10; i + 1) {\n    i;\n}";
  roundtrip "for (int i = 0; i < n; i + 1) {}";
  roundtrip "for (x = 0; x < 10; x + 1) {\n    x;\n}";
  check "for (int i = 0; i < 10; i + 1) {\n    return;\n}"
    "for (int i = 0; i < 10; i + 1) return;";
  roundtrip "for (;;) {}";
  roundtrip "for (int i = 0;;) {}";
  roundtrip "for (; i < 10;) {}";
  roundtrip "for (;; ++i) {}";
  roundtrip "for (int i; i < 10;) {}";
  roundtrip "for (int i;; ++i) {}";
  roundtrip "for (; i < 10; ++i) {}";
  roundtrip "for (int i = 0; i < 10;) {}";
  roundtrip "for (int i = 0;; ++i) {}";
  ()

let test_type_helpers _ =
  let st = make_state "long int x;" in
  Parser.consume st TokLongKw;
  assert_equal Ast.VLong (Parser.parse_long_suffix `None st);
  let st = make_state "long long int x;" in
  Parser.consume st TokLongKw;
  assert_equal Ast.VLongLong (Parser.parse_long_suffix `None st);
  let st = make_state "char x;" in
  assert_equal Ast.VChar (Parser.parse_int_base `None st);
  let st = make_state "char x;" in
  assert_equal Ast.VUChar (Parser.parse_int_base `Unsigned st);
  let st = make_state "x;" in
  assert_equal Ast.VInt (Parser.parse_int_base `Signed st);
  assert_bool "bool token is a type keyword" (Parser.is_type_keyword TokBoolKw);
  assert_bool "if token is not a type keyword"
    (not (Parser.is_type_keyword TokIfKw))

(* cast: (type)expr *)
let test_cast _ =
  (* all type keywords work as cast targets *)
  roundtrip "(int)x;";
  roundtrip "(long)x;";
  roundtrip "(bool)x;";
  roundtrip "(char)x;";
  roundtrip "(short)x;";
  roundtrip "(unsigned int)x;";
  roundtrip "(unsigned long)x;";
  roundtrip "(int*)x;";
  roundtrip "(int**)x;";
  (* cast of an expression *)
  roundtrip "(int)(x + 1);";
  (* cast of a literal *)
  roundtrip "(long)5;";
  (* cast binds tighter than binary ops *)
  check "(int)x + 1;" "(int)(x) + 1;";
  (* nested cast *)
  roundtrip "(int)(long)x;";
  (* postfix binds tighter than casting *)
  check "(int)x++;" "(int)(x++);";
  (* unary binds tighter than of unary minus *)
  check "(int)-x;" "(int)(-x);";
  (* postfix binds tighter than cast and deref*)
  check "(int)*p++;" "(int)(*(p++));"

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
  "parser"
  >::: [
         "literals" >:: test_literals;
         "top_level_pos" >:: test_top_level_pos;
         "arithmetic" >:: test_arithmetic;
         "precedence" >:: test_precedence;
         "comparison" >:: test_comparison;
         "boolean_logic" >:: test_boolean_logic;
         "bitwise" >:: test_bitwise;
         "mixed_precedence" >:: test_mixed_precedence;
         "unary" >:: test_unary;
         "incdec" >:: test_incdec;
         "multiple_statements" >:: test_multiple_statements;
         "var_defs" >:: test_var_defs;
         "int_types" >:: test_int_types;
         "char_literal" >:: test_char_literal;
         "pointer_types" >:: test_pointer_types;
         "array" >:: test_array;
         "break_continue" >:: test_break_continue;
         "returns" >:: test_returns;
         "function_calls" >:: test_function_calls;
         "ternary" >:: test_ternary;
         "compound_statements" >:: test_compound_statements;
         "if" >:: test_if;
         "assign" >:: test_assign;
         "while" >:: test_while;
         "do_while" >:: test_do_while;
         "for" >:: test_for;
         "type_helpers" >:: test_type_helpers;
         "cast" >:: test_cast;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
