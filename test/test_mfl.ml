open OUnit2
open Mfl

let assert_parse_string_equal expected input =
  assert_equal expected (Parser.parse input |> Ast.pp_expr)

let assert_parse_fails err input =
  assert_raises (Parser.Parse_error err) (fun () ->
      Parser.parse input |> Ast.pp_expr)

let assert_interpret_equals expected input =
  assert_equal ~printer:Ast.pp_expr (Ast.IntLiteral expected)
    (Interpreter.interpret (Parser.parse input))

let test_parse _ =
  assert_equal ~printer:Ast.pp_expr
    (Ast.BinaryOp (Ast.Add, Ast.IntLiteral 1, Ast.IntLiteral 2))
    (Parser.parse "1 + 2");

  assert_parse_string_equal "1 + 2 * 3" "1+2*3";
  assert_parse_string_equal "(1 + 2) * 3" "(1 + 2) * 3";
  assert_parse_string_equal "8 / (4 / 2)" "8 / (4 / 2)";
  assert_parse_string_equal "7 - (2 - 1)" "7 - (2 - 1)";
  assert_parse_fails "unexpected end of input" "";
  assert_parse_fails "unexpected end of input" "1 +";
  assert_parse_fails "unexpected trailing input" "1 + 2 3"

let test_interpret _ =
  assert_interpret_equals 3 "1 + 2";
  assert_interpret_equals 7 "1 + 2 * 3";
  assert_interpret_equals 9 "(1 + 2) * 3";
  assert_interpret_equals 4 "8 / (4 / 2)";
  assert_interpret_equals 6 "7 - (2 - 1)"

let tests =
  "test suite"
  >::: [ "test_parse" >:: test_parse; "test_interpret" >:: test_interpret ]

let _ = run_test_tt_main tests
