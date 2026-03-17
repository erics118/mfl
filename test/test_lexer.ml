open OUnit2
open Mfl

(* Lex an entire input string into a token list, stopping at Eof. Parser streams
   the tokens instead, lexing only as needed *)
let tokenize input =
  let st = Lexer.create input in
  let rec loop () =
    match Lexer.gettok st with
    | Token.Eof -> []
    | tok -> tok :: loop ()
  in
  loop ()

let check expected input =
  assert_equal
    ~printer:(fun ts ->
      "[" ^ String.concat "; " (List.map Token.string_of_token ts) ^ "]")
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

let test_identifiers_and_keywords _ =
  check [ IntKw ] "int";
  check [ BoolKw ] "bool";
  check [ ReturnKw ] "return";
  check [ IfKw ] "if";
  check [ ElseKw ] "else";
  check [ WhileKw ] "while";
  check [ ForKw ] "for";
  check [ VoidKw ] "void";
  check [ Identifier "x" ] "x";
  check [ Identifier "CustomType" ] "CustomType";
  check [ Identifier "abc1" ] "abc1";
  check [ Identifier "_a" ] "_a";
  check [ IntKw; Identifier "x"; Assign; Integer 3; Semicolon ] "int x = 3;";
  check
    [ BoolKw; Identifier "x"; Assign; Bool true; Semicolon ]
    "bool x = true;";
  check
    [ Identifier "CustomType"; Identifier "x"; Assign; Integer 3; Semicolon ]
    "CustomType x = 3;"

let test_parens _ =
  check [ LParen ] "(";
  check [ RParen ] ")";
  check [ Comma ] ",";
  check [ QuestionMark ] "?";
  check [ Colon ] ":";
  check [ LParen; Integer 1; RParen ] "(1)";
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
  check [ Integer 1; BinaryOp "+"; Integer 3; Semicolon ] "1 + 3;";
  check
    [
      Identifier "a";
      QuestionMark;
      Identifier "b";
      Colon;
      Identifier "c";
      Semicolon;
    ]
    "a ? b : c;";
  check [ ReturnKw; Integer 1; Semicolon ] "return 1;";
  check
    [
      IntKw;
      Identifier "f";
      LParen;
      IntKw;
      Identifier "a";
      Comma;
      IntKw;
      Identifier "b";
      RParen;
      LBrace;
      ReturnKw;
      Identifier "a";
      BinaryOp "+";
      Identifier "b";
      Semicolon;
      RBrace;
    ]
    "int f(int a, int b) { return a + b; }"

let test_string_of_token _ =
  assert_equal "EOF" (Token.string_of_token Eof);
  assert_equal ";" (Token.string_of_token Semicolon);
  assert_equal "," (Token.string_of_token Comma);
  assert_equal "?" (Token.string_of_token QuestionMark);
  assert_equal ":" (Token.string_of_token Colon);
  assert_equal "{" (Token.string_of_token LBrace);
  assert_equal "}" (Token.string_of_token RBrace);
  assert_equal "(" (Token.string_of_token LParen);
  assert_equal ")" (Token.string_of_token RParen);
  assert_equal "42" (Token.string_of_token (Integer 42));
  assert_equal "true" (Token.string_of_token (Bool true));
  assert_equal "int" (Token.string_of_token IntKw);
  assert_equal "bool" (Token.string_of_token BoolKw);
  assert_equal "return" (Token.string_of_token ReturnKw);
  assert_equal "if" (Token.string_of_token IfKw);
  assert_equal "else" (Token.string_of_token ElseKw);
  assert_equal "while" (Token.string_of_token WhileKw);
  assert_equal "for" (Token.string_of_token ForKw);
  assert_equal "x" (Token.string_of_token (Identifier "x"));
  assert_equal "+" (Token.string_of_token (BinaryOp "+"));
  assert_equal "!" (Token.string_of_token (UnaryOp "!"));
  assert_equal "=" (Token.string_of_token Assign)

let test_errors _ =
  lex_fails "invalid numeric literal '1a'" "1a";
  lex_fails "invalid numeric literal '123abc'" "123abc";
  lex_fails "invalid numeric literal '123a4'" "123a4";
  lex_fails "unexpected character '['" "[";
  lex_fails "unexpected character '@'" "@";
  lex_fails "unexpected character '#'" "#"

let tests =
  "lexer"
  >::: [
         "semicolons" >:: test_semicolons;
         "literals" >:: test_literals;
         "identifiers_and_keywords" >:: test_identifiers_and_keywords;
         "parens" >:: test_parens;
         "binary_ops" >:: test_binary_ops;
         "unary_ops" >:: test_unary_ops;
         "whitespace" >:: test_whitespace;
         "sequences" >:: test_sequences;
         "string_of_token" >:: test_string_of_token;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
