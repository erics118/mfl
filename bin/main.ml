open Mfl

let () =
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    prerr_endline "Usage: mfl \"EXPRESSION\" [output.ll]";
    exit 1);
  let input = Sys.argv.(1) in
  let outfile = if argc >= 3 then Sys.argv.(2) else "output.ll" in
  try
    let parsed = Parser.parse input in
    let i32 = Llvm.i32_type Codegen.context in
    let i8p = Llvm.pointer_type Codegen.context in
    let printf_ty = Llvm.var_arg_function_type i32 [| i8p |] in
    let printf_fn =
      Llvm.declare_function "printf" printf_ty Codegen.the_module
    in
    let main_fn =
      Llvm.declare_function "main"
        (Llvm.function_type i32 [||])
        Codegen.the_module
    in
    Llvm.position_at_end
      (Llvm.append_block Codegen.context "entry" main_fn)
      Codegen.builder;
    let result = Codegen.expr_to_llvm parsed in
    let fmt = Llvm.build_global_stringptr "%ld\n" "fmt" Codegen.builder in
    let _ =
      Llvm.build_call printf_ty printf_fn [| fmt; result |] "" Codegen.builder
    in
    let _ = Llvm.build_ret (Llvm.const_int i32 0) Codegen.builder in
    Llvm.print_module outfile Codegen.the_module;
    Printf.printf "Wrote %s\n" outfile
  with Failure msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
