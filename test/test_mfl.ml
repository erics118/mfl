open OUnit2
open Mfl

let check_parse input expected =
  assert_equal expected (Parser.parse input |> Ast.pp_expr)

let check_fails err input =
  assert_raises (Parser.Parse_error err) (fun () ->
      Parser.parse input |> Ast.pp_expr)

let test_parse _ =
  check_parse "1+2*3" "1 + 2 * 3";
  check_parse "(1 + 2) * 3" "(1 + 2) * 3";
  check_parse "8 / (4 / 2)" "8 / (4 / 2)";
  check_parse "7 - (2 - 1)" "7 - (2 - 1)";
  check_parse "3.5 + 1.25" "3.5 + 1.25";
  check_fails "unexpected end of input" "";
  check_fails "unexpected end of input" "1 +";
  check_fails "unexpected trailing input" "1 + 2 3"

let tests = "test suite" >::: [ "test_parse" >:: test_parse ]
let _ = run_test_tt_main tests
