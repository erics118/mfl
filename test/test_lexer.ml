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
  | exception Lexer.Lex_error (_, m) -> assert_equal ~printer:Fun.id err m

let test_semicolons _ =
  check [] "";
  check [ TokSemicolon ] ";";
  check [ TokSemicolon; TokSemicolon; TokSemicolon; TokSemicolon ] ";;;  ;"

let test_literals _ =
  check [ TokInt 0 ] "0";
  check [ TokInt 42 ] "42";
  check [ TokInt 123 ] "123";
  check [ TokDouble 1.0 ] "1.0";
  check [ TokDouble 0.3 ] "0.3";
  check [ TokDouble 1.0 ] "1.";
  check [ TokDouble 0.5 ] ".5";
  check [ TokLongDouble 1.0 ] "1.0L";
  check [ TokLongDouble 0.5 ] ".5L";
  check [ TokFloat 1.0 ] "1.0f";
  check [ TokFloat 1.0 ] "1.f";
  check [ TokFloat 0.5 ] ".5f";
  check [ TokBool true ] "true";
  check [ TokBool false ] "false"

let test_identifiers_and_keywords _ =
  check [ TokIntKw ] "int";
  check [ TokBoolKw ] "bool";
  check [ TokReturnKw ] "return";
  check [ TokIfKw ] "if";
  check [ TokElseKw ] "else";
  check [ TokWhileKw ] "while";
  check [ TokDoKw ] "do";
  check [ TokForKw ] "for";
  check [ TokVoidKw ] "void";
  check [ TokBreakKw ] "break";
  check [ TokContinueKw ] "continue";
  check [ TokCharKw ] "char";
  check [ TokShortKw ] "short";
  check [ TokLongKw ] "long";
  check [ TokUnsignedKw ] "unsigned";
  check [ TokSignedKw ] "signed";
  check [ TokTypedefKw ] "typedef";
  check [ TokSizeofKw ] "sizeof";
  check [ TokStructKw ] "struct";
  check [ TokFloatKw ] "float";
  check [ TokDoubleKw ] "double";
  check [ TokIdent "x" ] "x";
  check [ TokIdent "CustomType" ] "CustomType";
  check [ TokIdent "abc1" ] "abc1";
  check [ TokIdent "_a" ] "_a";
  check [ TokInt 0 ] "NULL";
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
  check [ TokPlus ] "+";
  check [ TokMinus ] "-";
  check [ TokStar ] "*";
  check [ TokSlash ] "/";
  check [ TokPercent ] "%";
  check [ TokLt ] "<";
  check [ TokGt ] ">";
  check [ TokEqEq ] "==";
  check [ TokBangEq ] "!=";
  check [ TokLtEq ] "<=";
  check [ TokGtEq ] ">=";
  check [ TokAmpAmp ] "&&";
  check [ TokPipePipe ] "||";
  check [ TokAmp ] "&";
  check [ TokPipe ] "|";
  check [ TokCaret ] "^";
  check [ TokLtLt ] "<<";
  check [ TokGtGt ] ">>";
  (* << and >> take priority over < and > *)
  check [ TokLtLt; TokInt 1 ] "<< 1";
  check [ TokGtGt; TokInt 1 ] ">> 1";
  (* <= and >= still work *)
  check [ TokLtEq ] "<=";
  check [ TokGtEq ] ">="

let test_member_access_ops _ =
  check [ TokDot ] ".";
  check [ TokArrow ] "->";
  (* -> takes priority over - and > separately *)
  check [ TokArrow; TokIdent "x" ] "->x";
  check [ TokMinus; TokGt ] "- >";
  (* . does not consume subsequent chars *)
  check [ TokDot; TokIdent "x" ] ".x"

let test_unary_ops _ =
  check [ TokBang ] "!";
  check [ TokTilde ] "~";
  check [ TokPlusPlus ] "++";
  check [ TokMinusMinus ] "--";
  (* != takes priority over ! *)
  check [ TokBangEq ] "!=";
  (* ++ and -- take priority over + and - *)
  check [ TokPlusPlus; TokInt 1 ] "++ 1";
  check [ TokMinusMinus; TokInt 1 ] "-- 1";
  (* not parsed as ++ and -- *)
  check [ TokPlus ] "+";
  check [ TokMinus ] "-"

let test_char _ =
  check [ TokChar 0 ] "'\\0'";
  check [ TokChar 7 ] "'\\a'";
  check [ TokChar 8 ] "'\\b'";
  check [ TokChar 9 ] "'\\t'";
  check [ TokChar 10 ] "'\\n'";
  check [ TokChar 11 ] "'\\v'";
  check [ TokChar 12 ] "'\\f'";
  check [ TokChar 13 ] "'\\r'";
  check [ TokChar 32 ] "' '";
  check [ TokChar 34 ] "'\"'";
  check [ TokChar 39 ] "'\\''";
  check [ TokChar 47 ] "'/'";
  check [ TokChar 48 ] "'0'";
  check [ TokChar 65 ] "'A'";
  check [ TokChar 92 ] "'\\\\'";

  (* hex escapes *)
  check [ TokChar 0 ] "'\\x00'";
  check [ TokChar 1 ] "'\\x01'";
  check [ TokChar 254 ] "'\\xfe'";
  check [ TokChar 255 ] "'\\xff'";

  lex_fails "unrecognized escape sequence '\\e'" "'\\e'";
  lex_fails "empty hex escape sequence" "'\\x'";
  lex_fails "hex escape out of range" "'\\x100'"

let test_whitespace _ =
  check [ TokInt 1; TokPlus; TokInt 2 ] "1 + 2";
  check [ TokInt 1; TokPlus; TokInt 2 ] "1+2";
  check [ TokInt 1; TokPlus; TokInt 2 ] "  1  +  2  ";
  check [ TokInt 1; TokPlus; TokInt 2 ] "1\t+\n2\r"

let test_sequences _ =
  check [ TokBool true; TokAmpAmp; TokBool false ] "true && false";
  check [ TokBang; TokBool true ] "!true";
  check [ TokInt 1; TokEqEq; TokInt 1 ] "1 == 1";
  check [ TokInt 3; TokAmp; TokInt 5 ] "3 & 5";
  check [ TokInt 1; TokSemicolon ] "1;";
  check [ TokInt 1; TokPlus; TokInt 3; TokSemicolon ] "1 + 3;";
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
      TokPlus;
      TokIdent "b";
      TokSemicolon;
      TokRBrace;
    ]
    "int f(int a, int b) { return a + b; }"

let test_string_of_token _ =
  (* special *)
  assert_equal "EOF" (string_of_token TokEof);
  (* punctuation *)
  assert_equal ";" (string_of_token TokSemicolon);
  assert_equal "," (string_of_token TokComma);
  assert_equal "?" (string_of_token TokQuestion);
  assert_equal ":" (string_of_token TokColon);
  assert_equal "{" (string_of_token TokLBrace);
  assert_equal "}" (string_of_token TokRBrace);
  assert_equal "(" (string_of_token TokLParen);
  assert_equal ")" (string_of_token TokRParen);
  assert_equal "[" (string_of_token TokLBracket);
  assert_equal "]" (string_of_token TokRBracket);
  (* literals *)
  assert_equal "42" (string_of_token (TokInt 42));
  assert_equal "true" (string_of_token (TokBool true));
  assert_equal "false" (string_of_token (TokBool false));
  (* identifiers *)
  assert_equal "x" (string_of_token (TokIdent "x"));
  (* keywords *)
  assert_equal "int" (string_of_token TokIntKw);
  assert_equal "bool" (string_of_token TokBoolKw);
  assert_equal "void" (string_of_token TokVoidKw);
  assert_equal "return" (string_of_token TokReturnKw);
  assert_equal "if" (string_of_token TokIfKw);
  assert_equal "else" (string_of_token TokElseKw);
  assert_equal "while" (string_of_token TokWhileKw);
  assert_equal "for" (string_of_token TokForKw);
  assert_equal "break" (string_of_token TokBreakKw);
  assert_equal "continue" (string_of_token TokContinueKw);
  assert_equal "do" (string_of_token TokDoKw);
  assert_equal "char" (string_of_token TokCharKw);
  assert_equal "short" (string_of_token TokShortKw);
  assert_equal "long" (string_of_token TokLongKw);
  assert_equal "unsigned" (string_of_token TokUnsignedKw);
  assert_equal "signed" (string_of_token TokSignedKw);
  assert_equal "typedef" (string_of_token TokTypedefKw);
  assert_equal "struct" (string_of_token TokStructKw);
  assert_equal "." (string_of_token TokDot);
  assert_equal "->" (string_of_token TokArrow);
  (* binary operators *)
  assert_equal "+" (string_of_token TokPlus);
  assert_equal "-" (string_of_token TokMinus);
  assert_equal "*" (string_of_token TokStar);
  assert_equal "/" (string_of_token TokSlash);
  assert_equal "%" (string_of_token TokPercent);
  assert_equal "==" (string_of_token TokEqEq);
  assert_equal "!=" (string_of_token TokBangEq);
  assert_equal "<" (string_of_token TokLt);
  assert_equal "<=" (string_of_token TokLtEq);
  assert_equal ">" (string_of_token TokGt);
  assert_equal ">=" (string_of_token TokGtEq);
  assert_equal "&&" (string_of_token TokAmpAmp);
  assert_equal "||" (string_of_token TokPipePipe);
  assert_equal "&" (string_of_token TokAmp);
  assert_equal "|" (string_of_token TokPipe);
  assert_equal "^" (string_of_token TokCaret);
  assert_equal "<<" (string_of_token TokLtLt);
  assert_equal ">>" (string_of_token TokGtGt);
  (* unary operators *)
  assert_equal "!" (string_of_token TokBang);
  assert_equal "~" (string_of_token TokTilde);
  assert_equal "++" (string_of_token TokPlusPlus);
  assert_equal "--" (string_of_token TokMinusMinus);
  (* assignment *)
  assert_equal "=" (string_of_token TokAssign)

let test_comments _ =
  (* line comments are stripped *)
  check [] "// hello world";
  check [] "//hello world";
  check [] "//////hello world";
  check [] "//////";
  check [] "// hello world\n";
  check [ TokInt 1 ] "// hello world\n1";
  check [ TokInt 1; TokInt 2 ] "1 // hello world\n2";
  check [ TokInt 1 ] "1 // no newline at eof";
  (* block comments are stripped *)
  check [] "/* hello world */";
  check [] "/*hello world*/";
  check [] "/**/";
  check [] "/*****/";
  check [ TokInt 1 ] "/* hello world */ 1";
  check [ TokInt 1; TokInt 2 ] "1 /* hello world */ 2";
  check [ TokInt 1 ] "/* line1\nline2 */ 1";
  (* block comment w normal tokens *)
  check [ TokInt 1; TokPlus; TokInt 2 ] "1/**/+/**/2";
  check [ TokStar; TokSlash ] "/* outer */ * /";
  (* unterminated block comment *)
  lex_fails "unterminated block comment" "/* bad"

let test_errors _ =
  lex_fails "invalid numeric literal '1a'" "1a";
  lex_fails "invalid numeric literal '123abc'" "123abc";
  lex_fails "invalid numeric literal '123a4'" "123a4";
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
         "member_access_ops" >:: test_member_access_ops;
         "unary_ops" >:: test_unary_ops;
         "char" >:: test_char;
         "whitespace" >:: test_whitespace;
         "sequences" >:: test_sequences;
         "string_of_token" >:: test_string_of_token;
         "comments" >:: test_comments;
         "errors" >:: test_errors;
       ]

let _ = run_test_tt_main tests
