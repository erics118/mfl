open Mfl

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: mfl <FILE>";
    exit 1);
  let file = Sys.argv.(1) in
  let stmt =
    match read_file file with
    | s -> (
        match Parser.parse s with
        | stmt -> stmt
        | exception Parser.Parse_error msg ->
            Printf.eprintf "parse error: %s\n" msg;
            exit 1)
    | exception Sys_error msg ->
        Printf.eprintf "io error: %s\n" msg;
        exit 1
  in
  let stmts =
    match stmt with
    | Ast.CompoundStmt stmts -> stmts
    | _ -> [ stmt ]
  in
  Codegen.codegen_program stmts;
  print_string (Codegen.emit_ir ())
