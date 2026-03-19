open Mfl

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let parse_source path =
  let content =
    match read_file path with
    | s -> s
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmt =
    match Parser.parse content with
    | s -> s
    | exception Parser.Parse_error ({ line; col }, msg) ->
        Printf.eprintf "%s:%d:%d: parse error: %s\n" path line col msg;
        exit 1
  in
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
  let stmt =
    match Parser.parse content with
    | s -> s
    | exception Parser.Parse_error ({ line; col }, msg) ->
        Printf.eprintf "%s:%d:%d: parse error: %s\n" input line col msg;
        exit 1
  in
  print_string (Pretty.pp_stmt stmt ^ "\n")

let usage_msg =
  "Usage: mfl <command> <input-file>\n\n\
   Commands:\n\
  \  ir      Emit LLVM IR\n\
  \  format  Pretty-print the source"

let () =
  if Array.length Sys.argv < 3 then (
    prerr_endline usage_msg;
    exit 1);
  let cmd = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  match cmd with
  | "ir" -> run_ir input
  | "format" -> run_format input
  | _ ->
      Printf.eprintf "mfl: unknown command '%s'\n\n%s\n" cmd usage_msg;
      exit 1
