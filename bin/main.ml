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
  match read_file file |> Parser.parse |> Pretty.pp_stmt with
  | result -> print_endline result
  | exception Sys_error msg ->
      Printf.eprintf "io error: %s\n" msg;
      exit 1
  | exception Parser.Parse_error msg ->
      Printf.eprintf "parse error: %s\n" msg;
      exit 1
