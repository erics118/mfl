open Ast

type severity =
  | Error
  | Warning

type stage =
  | Lexer
  | Parser
  | Typechecker
  | Preprocessor

type t = {
  stage : stage;
  severity : severity;
  pos : pos;
  message : string;
  file : string option;
}

exception Raised of t

let make ?file ?(severity = Error) stage pos message =
  { stage; severity; pos; message; file }

let raise_error diagnostic = raise (Raised diagnostic)

let string_of_stage = function
  | Lexer -> "lexical"
  | Parser -> "parse"
  | Typechecker -> "type"
  | Preprocessor -> "preprocessor"

let string_of_severity = function
  | Error -> "error"
  | Warning -> "warning"

let pp ?default_file fmt diagnostic =
  let file =
    match diagnostic.file with
    | Some file -> Some file
    | None -> default_file
  in
  begin match file with
  | Some file ->
      Format.fprintf fmt "%s:%d:%d: " file diagnostic.pos.line
        diagnostic.pos.col
  | None -> Format.fprintf fmt "%d:%d: " diagnostic.pos.line diagnostic.pos.col
  end;
  Format.fprintf fmt "%s %s: %s"
    (string_of_stage diagnostic.stage)
    (string_of_severity diagnostic.severity)
    diagnostic.message

let to_string ?default_file diagnostic =
  Format.asprintf "%a" (pp ?default_file) diagnostic
