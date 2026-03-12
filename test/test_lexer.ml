open OUnit2
open Mfl

(* Lex an entire input string into a token list, stopping at Eof. Parser streams
   the tokens instead, lexing only as needed *)
let tokenize input =
  let st = Lexer.create input in
  let rec loop () =
    match Lexer.gettok st with
    | Lexer.Eof -> []
    | tok -> tok :: loop ()
  in
  loop ()

let check expected input =
  assert_equal
    ~printer:(fun ts ->
      "[" ^ String.concat "; " (List.map Lexer.string_of_token ts) ^ "]")
    expected (tokenize input)

let lex_fails err input =
  assert_raises (Lexer.Lex_error err) (fun () -> tokenize input)

let test_semicolons _ =
  check [] "";
  check [ Semicolon ] ";";
  check [ Semicolon; Semicolon; Semicolon; Semicolon ] ";;;  ;"

let test_literals _ =
  check [ Integer 0 ] "0";
  check [ Integer 42 ] "42";
  check [ Integer 123 ] "123";
  check [ Bool true ] "true";
  check [ Bool false ] "false"

let test_parens _ =
  check [ Lparen ] "(";
  check [ Rparen ] ")";
  check [ Lparen; Integer 1; Rparen ] "(1)";
  check [ LBrace ] "{";
  check [ RBrace ] "}";
  check [ LBrace; Integer 1; Semicolon; RBrace ] "{1;}"

let test_binary_ops _ =
  check [ BinaryOp "+" ] "+";
  check [ BinaryOp "-" ] "-";
  check [ BinaryOp "*" ] "*";
  check [ BinaryOp "/" ] "/";
  check [ BinaryOp "%" ] "%";
  check [ BinaryOp "<" ] "<";
  check [ BinaryOp ">" ] ">";
  check [ BinaryOp "==" ] "==";
  check [ BinaryOp "!=" ] "!=";
  check [ BinaryOp "<=" ] "<=";
  check [ BinaryOp ">=" ] ">=";
  check [ BinaryOp "&&" ] "&&";
  check [ BinaryOp "||" ] "||";
  check [ BinaryOp "&" ] "&";
  check [ BinaryOp "|" ] "|";
  check [ BinaryOp "^" ] "^"

let test_unary_ops _ =
  check [ UnaryOp "!" ] "!";
  (* != takes priority over ! *)
  check [ BinaryOp "!=" ] "!="

let test_whitespace _ =
  check [ Integer 1; BinaryOp "+"; Integer 2 ] "1 + 2";
  check [ Integer 1; BinaryOp "+"; Integer 2 ] "1+2";
  check [ Integer 1; BinaryOp "+"; Integer 2 ] "  1  +  2  ";
  check [ Integer 1; BinaryOp "+"; Integer 2 ] "1\t+\n2\r"

let test_sequences _ =
  check [ Bool true; BinaryOp "&&"; Bool false ] "true && false";
  check [ UnaryOp "!"; Bool true ] "!true";
  check [ Integer 1; BinaryOp "=="; Integer 1 ] "1 == 1";
  check [ Integer 3; BinaryOp "&"; Integer 5 ] "3 & 5";
  check [ Integer 1; Semicolon ] "1;";
  check [ Integer 1; BinaryOp "+"; Integer 3; Semicolon ] "1 + 3;"

let test_string_of_token _ =
  assert_equal "EOF" (Lexer.string_of_token Eof);
  assert_equal ";" (Lexer.string_of_token Semicolon);
  assert_equal "{" (Lexer.string_of_token LBrace);
  assert_equal "}" (Lexer.string_of_token RBrace);
  assert_equal "(" (Lexer.string_of_token Lparen);
  assert_equal ")" (Lexer.string_of_token Rparen);
  assert_equal "42" (Lexer.string_of_token (Integer 42));
  assert_equal "true" (Lexer.string_of_token (Bool true));
  assert_equal "+" (Lexer.string_of_token (BinaryOp "+"));
  assert_equal "!" (Lexer.string_of_token (UnaryOp "!"))

let test_errors _ =
  lex_fails "unknown identifier 'hi'" "hi";
  lex_fails "unknown identifier 'A'" "A";
  lex_fails "unknown identifier 'a'" "1a";
  lex_fails "unknown identifier 'abc1'" "abc1";
  lex_fails "unknown identifier 'true1'" "true1";
  lex_fails "unknown identifier 'false2'" "false2";
  lex_fails "unknown identifier '_'" "_";
  lex_fails "unknown identifier 'foo'" "foo";
  lex_fails "unexpected character '['" "[";
  lex_fails "unexpected character '@'" "@";
  lex_fails "unexpected character '#'" "#"

let tests =
  "lexer"
  >::: [
         "semicolons" >:: test_semicolons;
         "literals" >:: test_literals;
         "parens" >:: test_parens;
         "binary_ops" >:: test_binary_ops;
         "unary_ops" >:: test_unary_ops;
         "whitespace" >:: test_whitespace;
         "sequences" >:: test_sequences;
         "string_of_token" >:: test_string_of_token;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
