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
        Printf.printf "%s\n" (Ast.pp_expr parsed)
      with Failure msg ->
        Printf.eprintf "Parse error: %s\n" msg;
        exit 1)
