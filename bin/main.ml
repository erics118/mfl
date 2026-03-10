open Mfl

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: mfl \"EXPRESSION\"";
    exit 1);
  let input = Sys.argv.(1) in
  match Parser.parse input |> Interpreter.interpret |> Ast.pp_expr with
  | result -> print_endline result
  | exception Parser.Parse_error msg ->
      Printf.eprintf "parse error: %s\n" msg;
      exit 1
  | exception Failure msg ->
      Printf.eprintf "error: %s\n" msg;
      exit 1
