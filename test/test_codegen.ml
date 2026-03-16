open OUnit2
open Mfl

let ir_contains ir fragment =
  let n = String.length ir and m = String.length fragment in
  let rec loop i =
    if i > n - m then false
    else if String.sub ir i m = fragment then true
    else loop (i + 1)
  in
  loop 0

let assert_ir_contains ir fragment =
  assert_bool
    (Printf.sprintf "IR missing %S\nFull IR:\n%s" fragment ir)
    (ir_contains ir fragment)

let codegen input =
  let stmt = Parser.parse input in
  let stmts =
    match stmt with
    | Ast.CompoundStmt s -> s
    | s -> [ s ]
  in
  Codegen.codegen_program stmts;
  Codegen.emit_ir ()

let test_main_return_zero _ =
  let ir = codegen "int main() {\n    return 0;\n}" in
  assert_ir_contains ir "define i32 @main()";
  assert_ir_contains ir "ret i32"

let tests = "codegen" >::: [ "main_return_zero" >:: test_main_return_zero ]
let _ = run_test_tt_main tests
