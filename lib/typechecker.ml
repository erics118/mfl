open Ast
include Typechecker_env
include Typechecker_error
include Typechecker_expr
include Typechecker_stmt
include Typechecker_types

let typecheck_program (stmts : parsed stmt list) : checked stmt list =
  let funcs = Hashtbl.create 8 in
  let typedefs = [ Hashtbl.create 8 ] in
  let structs = Hashtbl.create 8 in
  (* "stdlib" functions *)
  Hashtbl.replace funcs "printint" { params = [ Int ]; ret = Void };
  Hashtbl.replace funcs "printbool" { params = [ Bool ]; ret = Void };
  Hashtbl.replace funcs "malloc" { params = [ Long ]; ret = Ptr Void };
  let env =
    {
      vars = [ Hashtbl.create 8 ];
      funcs;
      typedefs;
      structs;
      return_typ = None;
      in_loop = false;
    }
  in
  List.map (typecheck_stmt env) stmts
