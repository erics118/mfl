open OUnit2
open Mfl
open Token

(* Lex an entire input string into a token list, stopping at TokEof. Parser
   streams the tokens instead, lexing only as needed *)
let tokenize input =
  let st = Lexer.create input in
  let rec loop () =
    match Lexer.next_token st with
    | TokEof -> []
    | tok -> tok :: loop ()
  in
  loop ()

let check expected input =
  assert_equal
    ~printer:(fun ts ->
      "[" ^ String.concat "; " (List.map string_of_token ts) ^ "]")
    expected (tokenize input)

let lex_fails err input =
  match tokenize input with
  | _ -> assert_failure (Printf.sprintf "expected Lex_error for: %s" input)
  | exception (Lexer.Lex_error (_, m)) -> assert_equal ~printer:Fun.id err m

let test_semicolons _ =
  check [] "";
  check [ TokSemicolon ] ";";
  check [ TokSemicolon; TokSemicolon; TokSemicolon; TokSemicolon ] ";;;  ;"

let test_literals _ =
  check [ TokInt 0 ] "0";
  check [ TokInt 42 ] "42";
  check [ TokInt 123 ] "123";
  check [ TokBool true ] "true";
  check [ TokBool false ] "false"

let test_identifiers_and_keywords _ =
  check [ TokIntKw ] "int";
  check [ TokBoolKw ] "bool";
  check [ TokReturnKw ] "return";
  check [ TokIfKw ] "if";
  check [ TokElseKw ] "else";
  check [ TokWhileKw ] "while";
  check [ TokForKw ] "for";
  check [ TokVoidKw ] "void";
  check [ TokIdent "x" ] "x";
  check [ TokIdent "CustomType" ] "CustomType";
  check [ TokIdent "abc1" ] "abc1";
  check [ TokIdent "_a" ] "_a";
  check
    [ TokIntKw; TokIdent "x"; TokAssign; TokInt 3; TokSemicolon ]
    "int x = 3;";
  check
    [ TokBoolKw; TokIdent "x"; TokAssign; TokBool true; TokSemicolon ]
    "bool x = true;";
  check
    [ TokIdent "CustomType"; TokIdent "x"; TokAssign; TokInt 3; TokSemicolon ]
    "CustomType x = 3;"

let test_parens _ =
  check [ TokLParen ] "(";
  check [ TokRParen ] ")";
  check [ TokComma ] ",";
  check [ TokQuestion ] "?";
  check [ TokColon ] ":";
  check [ TokLParen; TokInt 1; TokRParen ] "(1)";
  check [ TokLBrace ] "{";
  check [ TokRBrace ] "}";
  check [ TokLBrace; TokInt 1; TokSemicolon; TokRBrace ] "{1;}"

let test_binary_ops _ =
  check [ TokBinaryOp "+" ] "+";
  check [ TokBinaryOp "-" ] "-";
  check [ TokBinaryOp "*" ] "*";
  check [ TokBinaryOp "/" ] "/";
  check [ TokBinaryOp "%" ] "%";
  check [ TokBinaryOp "<" ] "<";
  check [ TokBinaryOp ">" ] ">";
  check [ TokBinaryOp "==" ] "==";
  check [ TokBinaryOp "!=" ] "!=";
  check [ TokBinaryOp "<=" ] "<=";
  check [ TokBinaryOp ">=" ] ">=";
  check [ TokBinaryOp "&&" ] "&&";
  check [ TokBinaryOp "||" ] "||";
  check [ TokBinaryOp "&" ] "&";
  check [ TokBinaryOp "|" ] "|";
  check [ TokBinaryOp "^" ] "^";
  check [ TokBinaryOp "<<" ] "<<";
  check [ TokBinaryOp ">>" ] ">>";
  (* << and >> take priority over < and > *)
  check [ TokBinaryOp "<<"; TokInt 1 ] "<< 1";
  check [ TokBinaryOp ">>"; TokInt 1 ] ">> 1";
  (* <= and >= still work *)
  check [ TokBinaryOp "<=" ] "<=";
  check [ TokBinaryOp ">=" ] ">="

let test_unary_ops _ =
  check [ TokUnaryOp "!" ] "!";
  check [ TokUnaryOp "~" ] "~";
  (* != takes priority over ! *)
  check [ TokBinaryOp "!=" ] "!="

let test_whitespace _ =
  check [ TokInt 1; TokBinaryOp "+"; TokInt 2 ] "1 + 2";
  check [ TokInt 1; TokBinaryOp "+"; TokInt 2 ] "1+2";
  check [ TokInt 1; TokBinaryOp "+"; TokInt 2 ] "  1  +  2  ";
  check [ TokInt 1; TokBinaryOp "+"; TokInt 2 ] "1\t+\n2\r"

let test_sequences _ =
  check [ TokBool true; TokBinaryOp "&&"; TokBool false ] "true && false";
  check [ TokUnaryOp "!"; TokBool true ] "!true";
  check [ TokInt 1; TokBinaryOp "=="; TokInt 1 ] "1 == 1";
  check [ TokInt 3; TokBinaryOp "&"; TokInt 5 ] "3 & 5";
  check [ TokInt 1; TokSemicolon ] "1;";
  check [ TokInt 1; TokBinaryOp "+"; TokInt 3; TokSemicolon ] "1 + 3;";
  check
    [
      TokIdent "a";
      TokQuestion;
      TokIdent "b";
      TokColon;
      TokIdent "c";
      TokSemicolon;
    ]
    "a ? b : c;";
  check [ TokReturnKw; TokInt 1; TokSemicolon ] "return 1;";
  check
    [
      TokIntKw;
      TokIdent "f";
      TokLParen;
      TokIntKw;
      TokIdent "a";
      TokComma;
      TokIntKw;
      TokIdent "b";
      TokRParen;
      TokLBrace;
      TokReturnKw;
      TokIdent "a";
      TokBinaryOp "+";
      TokIdent "b";
      TokSemicolon;
      TokRBrace;
    ]
    "int f(int a, int b) { return a + b; }"

let test_string_of_token _ =
  assert_equal "EOF" (string_of_token TokEof);
  assert_equal ";" (string_of_token TokSemicolon);
  assert_equal "," (string_of_token TokComma);
  assert_equal "?" (string_of_token TokQuestion);
  assert_equal ":" (string_of_token TokColon);
  assert_equal "{" (string_of_token TokLBrace);
  assert_equal "}" (string_of_token TokRBrace);
  assert_equal "(" (string_of_token TokLParen);
  assert_equal ")" (string_of_token TokRParen);
  assert_equal "42" (string_of_token (TokInt 42));
  assert_equal "true" (string_of_token (TokBool true));
  assert_equal "int" (string_of_token TokIntKw);
  assert_equal "bool" (string_of_token TokBoolKw);
  assert_equal "return" (string_of_token TokReturnKw);
  assert_equal "if" (string_of_token TokIfKw);
  assert_equal "else" (string_of_token TokElseKw);
  assert_equal "while" (string_of_token TokWhileKw);
  assert_equal "for" (string_of_token TokForKw);
  assert_equal "x" (string_of_token (TokIdent "x"));
  assert_equal "+" (string_of_token (TokBinaryOp "+"));
  assert_equal "!" (string_of_token (TokUnaryOp "!"));
  assert_equal "=" (string_of_token TokAssign)

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
