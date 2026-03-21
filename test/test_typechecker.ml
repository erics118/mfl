open OUnit2
open Mfl
open Ast
open Typechecker

let p = Parsed dummy_pos

let make_tbl pairs =
  let tbl = Hashtbl.create 4 in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) pairs;
  tbl

let default_env =
  {
    vars = [ Hashtbl.create 4 ];
    funcs = make_tbl [ ("noop", { params = []; ret = Void }) ];
    return_typ = None;
    in_loop = false;
  }

let i n = IntLiteral (p, n)
let b b = BoolLiteral (p, b)
let bi op l r = BinaryOp (p, op, l, r)
let un op e = UnaryOp (p, op, e)
let tern cond then_e else_e = Ternary (p, cond, then_e, else_e)

(* we aren't using mutable stuff here, so we shadow ! to mean variable reference
   and := to mean variable assignment, and we add $ to mean function
   call/application *)
let ( ! ) x = VarRef (p, x)
let ( := ) x e = Assign (p, !x, e)
let ( $ ) f args = FuncCall (p, f, args)
let cast ty e = Cast (p, ty, e)
let pre_inc e = PreInc (p, e)
let post_inc e = PostInc (p, e)
let pre_dec e = PreDec (p, e)
let post_dec e = PostDec (p, e)

(* noop function call, used to get a void type *)
let noop = "noop" $ []
let env_with vars = { default_env with vars = [ make_tbl vars ] }

let env_with_funcs funcs =
  {
    default_env with
    funcs = make_tbl (funcs @ [ ("noop", { params = []; ret = Void }) ]);
  }

let all_integer_types =
  [
    Bool;
    Char;
    UChar;
    Short;
    UShort;
    Int;
    UInt;
    Long;
    ULong;
    LongLong;
    ULongLong;
  ]

let name_of_typ = function
  | Bool -> "b"
  | Char -> "c"
  | UChar -> "uc"
  | Short -> "s"
  | UShort -> "us"
  | Int -> "i"
  | UInt -> "u"
  | Long -> "l"
  | ULong -> "ul"
  | LongLong -> "ll"
  | ULongLong -> "ull"
  | Ptr _ -> "p"
  | Void -> "v"

let all_integer_vars = List.map (fun t -> (name_of_typ t, t)) all_integer_types

let expected_promoted_type t =
  if integer_rank t < integer_rank Int then Int else t

let check_typ ?(env = default_env) expected e =
  match typecheck_expr env e with
  | result -> assert_equal ~printer:string_of_typ expected (expr_typ result)
  | exception Type_error (_, err) -> assert_failure (string_of_type_error err)

let check_err ?(env = default_env) expected_err e =
  match typecheck_expr env e with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let check_stmt_err ?(env = default_env) expected_err s =
  match typecheck_stmt env s with
  | _ -> assert_failure "expected Type_error"
  | exception Type_error (_, err) ->
      assert_equal ~printer:Fun.id expected_err (string_of_type_error err)

let assert_implicit_cast_to expected = function
  | ImplicitCast (Checked (_, t), _, _) ->
      assert_equal ~printer:string_of_typ expected t
  | _ -> assert_failure "expected ImplicitCast"

let test_literals _ =
  check_typ Int (i 0);
  check_typ Int (i 123);
  check_typ Int (i (-1));
  check_typ Bool (b true);
  check_typ Bool (b false)

let test_arithmetic _ =
  check_typ Int (bi Add (i 1) (i 2));
  check_typ Int (bi Sub (i 5) (i 3));
  check_typ Int (bi Mul (i 3) (i 4));
  check_typ Int (bi Div (i 8) (i 2));
  check_typ Int (bi Mod (i 7) (i 3))

let test_bitwise _ =
  check_typ Int (bi BitAnd (i 3) (i 5));
  check_typ Int (bi BitOr (i 3) (i 5));
  check_typ Int (bi BitXor (i 3) (i 5));
  check_typ Int (bi LShift (i 1) (i 2));
  check_typ Int (bi RShift (i 8) (i 1))

let test_comparison _ =
  check_typ Bool (bi Less (i 1) (i 2));
  check_typ Bool (bi Leq (i 1) (i 2));
  check_typ Bool (bi Greater (i 2) (i 1));
  check_typ Bool (bi Geq (i 2) (i 1))

let test_equality _ =
  check_typ Bool (bi Equal (i 1) (i 1));
  check_typ Bool (bi Neq (i 1) (i 2));
  check_typ Bool (bi Equal (b true) (b false));
  check_typ Bool (bi Neq (b true) (b true))

let test_logical _ =
  check_typ Bool (bi And (b true) (b false));
  check_typ Bool (bi Or (b false) (b true))

let test_unary _ =
  check_typ Bool (un Not (b true));
  check_typ Bool (un Not (b false));
  check_typ Bool (un Not (i 0));
  check_typ Int (un Neg (i 1));
  check_typ Int (un Compl (i 0))

let test_unary_conversions _ =
  let env = env_with all_integer_vars in
  List.iter
    (fun t ->
      let expected = expected_promoted_type t in
      let name = name_of_typ t in
      let not_e = typecheck_expr env (un Not !name) in
      let neg = typecheck_expr env (un Neg !name) in
      let compl = typecheck_expr env (un Compl !name) in
      assert_equal ~printer:string_of_typ Bool (expr_typ not_e);
      assert_equal ~printer:string_of_typ expected (expr_typ neg);
      assert_equal ~printer:string_of_typ expected (expr_typ compl);
      if expected <> t then begin
        let neg_operand =
          match neg with
          | UnaryOp (_, _, operand) -> operand
          | _ -> assert_failure "expected UnaryOp"
        in
        let compl_operand =
          match compl with
          | UnaryOp (_, _, operand) -> operand
          | _ -> assert_failure "expected UnaryOp"
        in
        assert_implicit_cast_to expected neg_operand;
        assert_implicit_cast_to expected compl_operand
      end)
    all_integer_types

let test_nested _ =
  (* (1 + 2) * 3 *)
  check_typ Int (bi Mul (bi Add (i 1) (i 2)) (i 3));
  (* !true && false *)
  check_typ Bool (bi And (un Not (b true)) (b false));
  (* 1 < 2 == true *)
  check_typ Bool (bi Equal (bi Less (i 1) (i 2)) (b true))

let test_arithmetic_errors _ =
  check_err "operator '+': type mismatch between 'void' and 'int'"
    (bi Add noop (i 1));
  check_err "operator '*': type mismatch between 'bool' and 'void'"
    (bi Mul (b true) noop)

let test_comparison_errors _ =
  check_err "operator '<': type mismatch between 'void' and 'bool'"
    (bi Less noop (b false));
  check_err "operator '>=': type mismatch between 'int' and 'void'"
    (bi Geq (i 1) noop)

let test_equality_errors _ =
  check_err "operator '==': type mismatch between 'void' and 'int'"
    (bi Equal noop (i 1));
  check_err "operator '!=': type mismatch between 'bool' and 'void'"
    (bi Neq (b false) noop)

let test_logical_errors _ =
  check_err "operator '&&': type mismatch between 'int' and 'int'"
    (bi And (i 1) (i 0));
  check_err "operator '||': type mismatch between 'bool' and 'int'"
    (bi Or (b true) (i 1))

let test_unary_errors _ =
  check_err "operator '!': invalid operand type 'void'" (un Not noop);
  check_err "operator '-': invalid operand type 'void'" (un Neg noop);
  check_err "operator '~': invalid operand type 'void'" (un Compl noop)

let test_var_ref _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int !"x";
  check_typ ~env Bool !"ok"

let test_var_ref_errors _ =
  check_err "unbound variable 'x'" !"x";
  check_err "unbound variable 'y'" !"y"

let test_assign _ =
  let env = env_with [ ("x", Int); ("ok", Bool) ] in
  check_typ ~env Int ("x" := i 42);
  check_typ ~env Bool ("ok" := b true);
  check_typ ~env Int ("x" := bi Add (i 1) (i 2))

let test_assign_errors _ =
  let env = env_with [ ("x", Int) ] in
  check_err ~env "unbound variable 'y'" ("y" := i 1);
  check_err ~env "expected type 'int' but got 'void'" ("x" := noop)

let test_func_call _ =
  let env =
    env_with_funcs
      [
        ("add", { params = [ Int; Int ]; ret = Int });
        ("ready", { params = []; ret = Bool });
      ]
  in
  check_typ ~env Int ("add" $ [ i 1; i 2 ]);
  check_typ ~env Bool ("ready" $ [])

let test_func_call_errors _ =
  let env = env_with_funcs [ ("add", { params = [ Int; Int ]; ret = Int }) ] in
  check_err ~env "unbound function 'f'" ("f" $ []);
  check_err ~env "'add' expects 2 argument(s) but got 1" ("add" $ [ i 1 ]);
  check_err ~env "'add' expects 2 argument(s) but got 0" ("add" $ []);
  check_err ~env "expected type 'int' but got 'void'" ("add" $ [ noop; i 1 ])

let test_ternary _ =
  check_typ Int (tern (b true) (i 1) (i 2));
  check_typ Bool (tern (b false) (b true) (b false));
  check_typ Bool (tern (bi Less (i 1) (i 2)) (b true) (b false))

let test_ternary_errors _ =
  check_err "condition must be 'bool' but got 'int'" (tern (i 1) (i 2) (i 3));
  check_err "expected type 'int' but got 'bool'" (tern (b true) (i 1) (b false));
  check_err "expected type 'bool' but got 'int'" (tern (b true) (b false) (i 1))

let test_incdec _ =
  let env = env_with [ ("x", Int); ("flag", Bool) ] in
  check_typ ~env Int (pre_inc !"x");
  check_typ ~env Int (post_inc !"x");
  check_typ ~env Int (pre_dec !"x");
  check_typ ~env Int (post_dec !"x");
  check_typ ~env Bool (pre_inc !"flag");
  check_typ ~env Bool (post_inc !"flag");
  check_typ ~env Bool (pre_dec !"flag");
  check_typ ~env Bool (post_dec !"flag")

let test_incdec_errors _ =
  let env_int = env_with [ ("x", Int) ] in
  (* non-lvalue operand *)
  check_err ~env:env_int "expression is not an lvalue" (pre_inc (i 1));
  check_err ~env:env_int "expression is not an lvalue"
    (post_inc (bi Add !"x" (i 1)));
  check_err "expression is not an lvalue" (pre_inc noop);
  check_err "expression is not an lvalue" (post_dec noop)

let test_cast _ =
  let env = env_with [ ("x", Long); ("n", Int); ("c", Char); ("p", Ptr Int) ] in
  check_typ ~env Int (cast VInt !"x");
  check_typ ~env Long (cast VLong !"n");
  check_typ ~env Bool (cast VBool !"n");
  check_typ ~env Char (cast VChar !"n");
  check_typ ~env (Ptr Int) (cast (VPtr VInt) !"n");
  check_typ ~env Int (cast VInt !"p");
  check_typ ~env (Ptr (Ptr Int)) (cast (VPtr (VPtr VInt)) !"p");
  check_typ Int (cast VInt (i 5));
  check_typ Long (cast VLong (i 5))

let test_cast_errors _ =
  check_err "cannot cast from 'int' to 'void'" (cast VVoid (i 1));
  check_err "cannot cast from 'void' to 'int'" (cast VInt noop)

let test_conversions _ =
  let env =
    env_with
      [
        ("c", Char);
        ("uc", UChar);
        ("s", Short);
        ("us", UShort);
        ("u", UInt);
        ("l", Long);
        ("ul", ULong);
        ("ll", LongLong);
      ]
  in
  check_typ ~env Int (bi Add !"c" (i 1));
  check_typ ~env Int (bi Add !"uc" !"s");
  check_typ ~env Int (bi Add !"us" !"s");
  check_typ ~env UInt (bi Add !"u" (i 1));
  check_typ ~env Long (bi Add !"l" !"u");
  check_typ ~env ULong (bi Add !"ul" !"l");
  check_typ ~env LongLong (bi Add !"ll" !"u");
  check_typ ~env ULongLong (bi Add !"ll" !"ul");
  check_typ ~env Int (bi LShift !"c" (i 1));
  check_typ ~env Int (bi RShift !"us" (b false));
  check_typ Bool (bi Equal (b true) (i 1));
  check_typ ~env Bool (bi Less !"ul" !"ll");
  check_typ Int (un Neg (b true));
  check_typ Int (un Compl (b false));
  let env = env_with [ ("ok", Bool) ] in
  check_typ ~env Bool ("ok" := i 7);
  let env =
    env_with_funcs
      [
        ("flag", { params = [ Bool ]; ret = Bool });
        ("widen", { params = [ Long ]; ret = Long });
      ]
  in
  check_typ ~env Bool ("flag" $ [ i 1 ]);
  check_typ ~env Long ("widen" $ [ i 1 ]);
  check_typ Int (cast VInt (b true));
  check_typ Bool (cast VBool (i 7));
  check_typ ULongLong (cast VULongLong (i 7))

let test_implicit_casts _ =
  let env = env_with [ ("ok", Bool); ("c", Char); ("l", Long) ] in
  (match typecheck_expr env ("ok" := i 3) with
  | Assign (_, _, value) -> assert_implicit_cast_to Bool value
  | _ -> assert_failure "expected Assign");
  match typecheck_expr env (bi Add !"c" !"l") with
  | BinaryOp (_, _, lhs, _) -> assert_implicit_cast_to Long lhs
  | _ -> assert_failure "expected BinaryOp"

let test_pointers _ =
  let env =
    {
      (env_with
         [
           ("x", Int);
           ("y", Int);
           ("px", Ptr Int);
           ("py", Ptr Int);
           ("pp", Ptr (Ptr Int));
         ])
      with
      funcs = make_tbl [ ("load", { params = [ Ptr Int ]; ret = Int }) ];
    }
  in
  check_typ ~env (Ptr Int) (un AddrOf !"x");
  check_typ ~env Int (un Deref !"px");
  check_typ ~env (Ptr (Ptr Int)) (un AddrOf (un Deref !"pp"));
  check_typ ~env Bool (bi Equal !"px" !"py");
  check_typ ~env (Ptr Int) ("px" := un AddrOf !"x");
  check_typ ~env Int ("load" $ [ un AddrOf !"x" ]);
  (* assign through a pointer *)
  check_typ ~env Int (Assign (p, un Deref !"px", i 42));
  check_typ ~env Int (Assign (p, un Deref !"px", !"x"));
  (* assign through a double pointer *)
  check_typ ~env (Ptr Int) (Assign (p, un Deref !"pp", !"px"))

let test_pointer_arithmetic _ =
  let env =
    env_with
      [
        ("x", Int);
        ("n", Int);
        ("c", Char);
        ("l", Long);
        ("px", Ptr Int);
        ("py", Ptr Int);
        ("pc", Ptr Char);
        ("pp", Ptr (Ptr Int));
      ]
  in
  (* ptr + int, int + ptr *)
  check_typ ~env (Ptr Int) (bi Add !"px" !"n");
  check_typ ~env (Ptr Int) (bi Add !"n" !"px");
  (* ptr + non-Int integer offset types *)
  check_typ ~env (Ptr Int) (bi Add !"px" !"c");
  check_typ ~env (Ptr Int) (bi Add !"px" !"l");
  check_typ ~env (Ptr Int) (bi Add !"c" !"px");
  check_typ ~env (Ptr Int) (bi Add !"l" !"px");
  (* ptr - int *)
  check_typ ~env (Ptr Int) (bi Sub !"px" !"n");
  check_typ ~env (Ptr Int) (bi Sub !"px" !"c");
  check_typ ~env (Ptr Int) (bi Sub !"px" !"l");
  (* ptr - ptr -> long *)
  check_typ ~env Long (bi Sub !"px" !"py");
  check_typ ~env Long (bi Sub !"pc" !"pc");
  (* pointer comparisons *)
  check_typ ~env Bool (bi Less !"px" !"py");
  check_typ ~env Bool (bi Leq !"px" !"py");
  check_typ ~env Bool (bi Greater !"px" !"py");
  check_typ ~env Bool (bi Geq !"px" !"py");
  check_typ ~env Bool (bi Equal !"px" !"py");
  check_typ ~env Bool (bi Neq !"px" !"py");
  (* ptr++ and ++ptr on int* *)
  check_typ ~env (Ptr Int) (post_inc !"px");
  check_typ ~env (Ptr Int) (pre_inc !"px");
  check_typ ~env (Ptr Int) (post_dec !"px");
  check_typ ~env (Ptr Int) (pre_dec !"px");
  (* ptr++ on a double pointer *)
  check_typ ~env (Ptr (Ptr Int)) (post_inc !"pp");
  check_typ ~env (Ptr (Ptr Int)) (pre_inc !"pp")

let test_pointer_arithmetic_errors _ =
  let env =
    env_with
      [
        ("x", Int);
        ("px", Ptr Int);
        ("py", Ptr Int);
        ("pc", Ptr Char);
        ("pp", Ptr (Ptr Int));
      ]
  in
  (* int - ptr is invalid *)
  check_err ~env "operator '-': type mismatch between 'int' and 'int*'"
    (bi Sub !"x" !"px");
  (* ptr + ptr is invalid *)
  check_err ~env "operator '+': type mismatch between 'int*' and 'int*'"
    (bi Add !"px" !"py");
  (* ptr - ptr of different types is invalid *)
  check_err ~env "operator '-': type mismatch between 'int*' and 'char*'"
    (bi Sub !"px" !"pc");
  (* ptr * int is invalid *)
  check_err ~env "operator '*': type mismatch between 'int*' and 'int'"
    (bi Mul !"px" !"x");
  (* pointer comparisons between different pointer types are invalid *)
  check_err ~env "operator '<': type mismatch between 'int*' and 'char*'"
    (bi Less !"px" !"pc");
  check_err ~env "operator '==': type mismatch between 'int*' and 'char*'"
    (bi Equal !"px" !"pc");
  ()

let test_pointer_errors _ =
  let env =
    env_with [ ("x", Int); ("p", Ptr Int); ("vp", Ptr Void); ("q", Ptr Int) ]
  in
  check_err ~env "expression is not an lvalue" (un AddrOf (bi Add !"x" (i 1)));
  check_err ~env "operator '*': invalid operand type 'int'" (un Deref !"x");
  check_err ~env "operator '*': invalid operand type 'void*'" (un Deref !"vp");
  check_err ~env "expected type 'int*' but got 'int'" ("p" := i 1);
  check_err ~env "expected type 'int*' but got 'bool'" ("p" := b true);
  (* assign wrong type through pointer: int* cannot be implicitly cast to int *)
  check_err ~env "expected type 'int' but got 'int*'"
    (Assign (p, un Deref !"p", !"p"))

let test_break_continue _ =
  (* break and continue are valid inside loops *)
  let loop_env = { default_env with in_loop = true } in
  (match typecheck_stmt loop_env (BreakStmt dummy_pos) with
  | BreakStmt _ -> ()
  | _ -> assert_failure "expected BreakStmt");
  match typecheck_stmt loop_env (ContinueStmt dummy_pos) with
  | ContinueStmt _ -> ()
  | _ -> assert_failure "expected ContinueStmt"

let test_var_type_resolution _ =
  let check vt =
    ignore
      (typecheck_stmt default_env
         (VarDef { pos = dummy_pos; var_type = vt; name = "x"; init = None }))
  in
  check VBool;
  check VChar;
  check VUChar;
  check VShort;
  check VUShort;
  check VInt;
  check VUInt;
  check VLong;
  check VULong;
  check VLongLong;
  check VULongLong;
  check (VPtr VInt);
  check (VPtr (VPtr VInt));
  (* VNamed with an unknown name must raise UnknownType *)
  check_stmt_err "unknown type 'Foo'"
    (VarDef
       { pos = dummy_pos; var_type = VNamed "Foo"; name = "x"; init = None })

let test_break_continue_errors _ =
  (* break and continue outside a loop are errors *)
  check_stmt_err "break statement outside of a loop" (BreakStmt dummy_pos);
  check_stmt_err "continue statement outside of a loop" (ContinueStmt dummy_pos)

let test_missing_return _ =
  check_stmt_err "control reaches end of non-void function 'f'"
    (FuncDef
       {
         pos = dummy_pos;
         ret_type = VInt;
         name = "f";
         params = [];
         body = [ EmptyStmt dummy_pos ];
       });
  ignore
    (typecheck_stmt default_env
       (FuncDef
          {
            pos = dummy_pos;
            ret_type = VInt;
            name = "f";
            params = [];
            body =
              [
                If
                  {
                    pos = dummy_pos;
                    cond = b true;
                    then_body = ReturnStmt (dummy_pos, Some (i 1));
                    else_body = Some (ReturnStmt (dummy_pos, Some (i 2)));
                  };
              ];
          }))

let tests =
  "typechecker"
  >::: [
         "literals" >:: test_literals;
         "arithmetic" >:: test_arithmetic;
         "bitwise" >:: test_bitwise;
         "comparison" >:: test_comparison;
         "equality" >:: test_equality;
         "logical" >:: test_logical;
         "unary" >:: test_unary;
         "unary_conversions" >:: test_unary_conversions;
         "nested" >:: test_nested;
         "arithmetic_errors" >:: test_arithmetic_errors;
         "comparison_errors" >:: test_comparison_errors;
         "equality_errors" >:: test_equality_errors;
         "conversions" >:: test_conversions;
         "logical_errors" >:: test_logical_errors;
         "unary_errors" >:: test_unary_errors;
         "var_ref" >:: test_var_ref;
         "var_ref_errors" >:: test_var_ref_errors;
         "assign" >:: test_assign;
         "assign_errors" >:: test_assign_errors;
         "func_call" >:: test_func_call;
         "func_call_errors" >:: test_func_call_errors;
         "ternary" >:: test_ternary;
         "ternary_errors" >:: test_ternary_errors;
         "var_type_resolution" >:: test_var_type_resolution;
         "cast" >:: test_cast;
         "cast_errors" >:: test_cast_errors;
         "implicit_casts" >:: test_implicit_casts;
         "pointers" >:: test_pointers;
         "pointer_errors" >:: test_pointer_errors;
         "pointer_arithmetic" >:: test_pointer_arithmetic;
         "pointer_arithmetic_errors" >:: test_pointer_arithmetic_errors;
         "break_continue" >:: test_break_continue;
         "break_continue_errors" >:: test_break_continue_errors;
         "missing_return" >:: test_missing_return;
         "incdec" >:: test_incdec;
         "incdec_errors" >:: test_incdec_errors;
       ]

let _ = run_test_tt_main tests
