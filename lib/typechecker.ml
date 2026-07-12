open Ast
include Typechecker_env
include Typechecker_error
include Typechecker_expr
include Typechecker_stmt
include Typechecker_types

let typecheck_program (stmts : parsed stmt list) : checked stmt list =
  let env =
    {
      vars = [ StringMap.empty ];
      typedefs = [ StringMap.empty ];
      globals = { funcs = StringMap.empty; structs = StringMap.empty };
      return_typ = None;
      in_loop = false;
    }
  in
  snd (typecheck_stmts env stmts)
