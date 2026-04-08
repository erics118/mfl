(** preprocessor *)

(** preprocessor exception *)
exception Preprocess_error of Ast.pos * string

(* data for preprocessor, will include #defines, etc in the future *)
type env = { include_dirs : string list }

let make_pos line = { Ast.line; col = 1 }

(* read a single file into a string *)
let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

(** search [dirs] for [name], returning the first path that exists *)
let find_in_dirs dirs name =
  List.find_map
    (fun dir ->
      let path = Filename.concat dir name in
      if Sys.file_exists path then Some path else None)
    dirs

(** handle [#include <name>] or [#include "name"] *)
let rec handle_include env ~file ~line_no rest buf =
  let rest = String.trim rest in
  let len = String.length rest in
  if len < 2 then
    raise (Preprocess_error (make_pos line_no, "malformed #include"));
  (* first determine delimiter style from opening char *)
  let close_char, local_style =
    match rest.[0] with
    | '<' -> ('>', false)
    | '"' -> ('"', true)
    | _ -> raise (Preprocess_error (make_pos line_no, "malformed #include"))
  in
  (* find the closing delimiter *)
  let close_pos =
    match String.index_from_opt rest 1 close_char with
    | None ->
        raise
          (Preprocess_error
             (make_pos line_no, "unterminated filename in #include"))
    | Some i -> i
  in
  let name = String.sub rest 1 (close_pos - 1) in
  if String.length name = 0 then
    raise (Preprocess_error (make_pos line_no, "empty filename in #include"));
  (* error if non-whitespace follows the delimiter *)
  (* todo: we should allow comments, or treat comments like whitespace and
     remove them from the ast entirely *)
  let after =
    String.trim (String.sub rest (close_pos + 1) (len - close_pos - 1))
  in
  if String.length after > 0 then
    raise
      (Preprocess_error
         (make_pos line_no, "detected extra tokens after #include filename"));
  (* for quoted includes, also search relative to the current file *)
  let search_dirs =
    if local_style then Filename.dirname file :: env.include_dirs
    else env.include_dirs
  in
  match find_in_dirs search_dirs name with
  | None ->
      raise
        (Preprocess_error
           (make_pos line_no, Printf.sprintf "file not found: %s" name))
  | Some path ->
      let content = read_file path in
      let expanded = process_file env ~file:path content in
      Buffer.add_string buf expanded;
      (* the line after #include is always a separate line, even if the included
         file lacks a final newline *)
      if
        String.length expanded > 0
        && expanded.[String.length expanded - 1] <> '\n'
      then Buffer.add_char buf '\n'

(* dispatch on directive name. will add #define, etc later *)
and handle_directive env ~file ~line_no name rest buf =
  match name with
  | "include" -> handle_include env ~file ~line_no rest buf
  | d ->
      raise
        (Preprocess_error
           (make_pos line_no, Printf.sprintf "unknown directive #%s" d))

(** process [source] from [file], expanding directives into [buf] *)
and process_file env ~file source =
  let lines = String.split_on_char '\n' source in
  let buf = Buffer.create (String.length source) in
  List.iteri
    (fun i line ->
      let line_no = i + 1 in
      let trimmed = String.trim line in
      if String.length trimmed > 0 && trimmed.[0] = '#' then begin
        (* parse directive name and remainder *)
        let after = String.sub trimmed 1 (String.length trimmed - 1) in
        let after = String.trim after in
        let name, rest =
          match String.index_opt after ' ' with
          | None -> (after, "")
          | Some i ->
              ( String.sub after 0 i,
                String.sub after (i + 1) (String.length after - i - 1) )
        in
        handle_directive env ~file ~line_no name rest buf;
        (* todo: emit #line so lexer positions stay accurate. right now, line
           numbers get shifted *)
        (* emit a newline to replace where the #include used to be, so line
        numbers work *)
        Buffer.add_char buf '\n'
      end
      else begin
        (* normal code, just copy the line over *)
        Buffer.add_string buf line;
        if i < List.length lines - 1 then Buffer.add_char buf '\n'
      end)
    lines;
  Buffer.contents buf

(** preprocess [source] from [file], searching [include_dirs] for headers *)
let preprocess ~include_dirs ~file source =
  let env = { include_dirs } in
  process_file env ~file source
