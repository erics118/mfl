open Mfl

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let preprocess ~include_dirs path content =
  match Preprocessor.preprocess ~include_dirs ~file:path content with
  | s -> s
  | exception Preprocessor.Preprocess_error ({ line; col }, msg) ->
      Printf.eprintf "%s:%d:%d: preprocessor error: %s\n" path line col msg;
      exit 1

let parse_with_errors path content =
  match Parser.parse content with
  | s -> s
  | exception Lexer.Lex_error ({ line; col }, msg) ->
      Printf.eprintf "%s:%d:%d: lexical error: %s\n" path line col msg;
      exit 1
  | exception Parser.Parse_error ({ line; col }, msg) ->
      Printf.eprintf "%s:%d:%d: parse error: %s\n" path line col msg;
      exit 1

let parse_source ~include_dirs path =
  let content =
    match read_file path with
    | s -> s
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmt = parse_with_errors path (preprocess ~include_dirs path content) in
  match stmt with
  | Ast.CompoundStmt (_, stmts) -> stmts
  | _ -> [ stmt ]

let typecheck stmts =
  match Typechecker.typecheck_program stmts with
  | s -> s
  | exception Typechecker.Type_error ({ line; col }, e) ->
      Printf.eprintf "%d:%d: type error: %s\n" line col
        (Typechecker.string_of_type_error e);
      exit 1

let run_ir ~include_dirs input =
  let stmts = parse_source ~include_dirs input |> typecheck in
  Codegen.codegen_program stmts;
  print_string (Codegen.emit_ir ())

let run_format ~include_dirs input =
  let content =
    match read_file input with
    | s -> s
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmt = parse_with_errors input (preprocess ~include_dirs input content) in
  print_string (Pretty.pp_stmt stmt ^ "\n")

let run_run ~include_dirs input =
  let stmts = parse_source ~include_dirs input |> typecheck in
  Codegen.codegen_program stmts;
  let ir = Codegen.emit_ir () in
  let ir_file = Filename.temp_file "mfl" ".ll" in
  let bin_file = Filename.temp_file "mfl" "" in
  let oc = open_out ir_file in
  output_string oc ir;
  close_out oc;
  let clang_exit =
    Sys.command
      (Printf.sprintf "clang -w %s -o %s" (Filename.quote ir_file)
         (Filename.quote bin_file))
  in
  Sys.remove ir_file;
  if clang_exit <> 0 then (
    (try Sys.remove bin_file with _ -> ());
    exit clang_exit);
  let run_exit = Sys.command (Filename.quote bin_file) in
  (try Sys.remove bin_file with _ -> ());
  exit run_exit

let usage_msg =
  {|Usage: mfl <command> [-I <dir>] <input-file>
Commands:
    ir        Emit LLVM IR
    format    Pretty-print the source
    run       Compile and execute|}

let () =
  let cmd, include_dirs, input =
    match Sys.argv with
    | [| _; cmd; "-I"; dir; file |] -> (cmd, [ dir; "include" ], file)
    | [| _; cmd; file |] -> (cmd, [ "include" ], file)
    | _ ->
        prerr_endline usage_msg;
        exit 1
  in
  match cmd with
  | "ir" -> run_ir ~include_dirs input
  | "format" -> run_format ~include_dirs input
  | "run" -> run_run ~include_dirs input
  | _ ->
      Printf.eprintf "mfl: unknown command '%s'\n\n%s\n" cmd usage_msg;
      exit 1
