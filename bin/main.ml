open Mfl

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let parse_with_errors path content =
  match Parser.parse content with
  | s -> s
  | exception Lexer.Lex_error ({ line; col }, msg) ->
      Printf.eprintf "%s:%d:%d: lexical error: %s\n" path line col msg;
      exit 1
  | exception Parser.Parse_error ({ line; col }, msg) ->
      Printf.eprintf "%s:%d:%d: parse error: %s\n" path line col msg;
      exit 1

let parse_source path =
  let content =
    match read_file path with
    | s -> s
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmt = parse_with_errors path content in
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

let run_ir input =
  let stmts = parse_source input |> typecheck in
  Codegen.codegen_program stmts;
  print_string (Codegen.emit_ir ())

let run_format input =
  let content =
    match read_file input with
    | s -> s
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmt = parse_with_errors input content in
  print_string (Pretty.pp_stmt stmt ^ "\n")

let run_run input =
  let stmts = parse_source input |> typecheck in
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
  {|Usage: mfl <command> <input-file>
Commands:
    ir      Emit LLVM IR
    format  Pretty-print the source
    run     Compile and execute|}

let () =
  if Array.length Sys.argv < 3 then (
    prerr_endline usage_msg;
    exit 1);
  let cmd = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  match cmd with
  | "ir" -> run_ir input
  | "format" -> run_format input
  | "run" -> run_run input
  | _ ->
      Printf.eprintf "mfl: unknown command '%s'\n\n%s\n" cmd usage_msg;
      exit 1
