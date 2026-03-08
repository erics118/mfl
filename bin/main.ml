open Mfl

let expression_from_argv () =
  let argc = Array.length Sys.argv in
  if argc < 2 then None
  else
    let words = Array.to_list (Array.sub Sys.argv 1 (argc - 1)) in
    Some (String.concat " " words)

let () =
  match expression_from_argv () with
  | None ->
      prerr_endline "Usage: mfl \"ARITHMETIC_EXPRESSION\"";
      exit 1
  | Some input -> (
      try
        let parsed = Parser.parse input in
        (* wrap expression in an anonymous `() -> double` function *)
        let ft = Llvm.function_type Codegen.double_type [||] in
        let fn = Llvm.declare_function "__anon" ft Codegen.the_module in
        let bb = Llvm.append_block Codegen.context "entry" fn in
        Llvm.position_at_end bb Codegen.builder;
        let ret = Codegen.expr_to_llvm parsed in
        let _ = Llvm.build_ret ret Codegen.builder in
        (* JIT and run *)
        ignore (Llvm_executionengine.initialize ());
        let engine = Llvm_executionengine.create Codegen.the_module in
        let fp =
          Llvm_executionengine.get_function_address "__anon"
            Foreign.(funptr Ctypes.(void @-> returning double))
            engine
        in
        Printf.printf "%g\n" (fp ())
      with Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1)
