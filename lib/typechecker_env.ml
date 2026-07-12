open Ast

type func_sig = {
  params : typ list;
  ret : typ;
  is_variadic : bool;
}

(* map of strings *)
module StringMap = Map.Make (String)

type globals = {
  funcs : func_sig StringMap.t;
  (* struct tags are currently program-wide rather than scoped *)
  structs : (string * typ) list StringMap.t;
}

type env = {
  (* head is innermost scope. push when entering a new block, discard on exit *)
  vars : typ StringMap.t list;
  typedefs : source_type StringMap.t list;
  globals : globals;
  return_typ : typ option;
  in_loop : bool;
}

let push_scope env =
  {
    env with
    vars = StringMap.empty :: env.vars;
    typedefs = StringMap.empty :: env.typedefs;
  }

let lookup scopes name =
  List.find_map (fun scope -> StringMap.find_opt name scope) scopes

let define scopes name value =
  match scopes with
  | scope :: rest -> StringMap.add name value scope :: rest
  | [] -> failwith "empty scope stack"

(* variables *)
let lookup_var env name = lookup env.vars name
let define_var env name typ = { env with vars = define env.vars name typ }

(* typedefs *)
let lookup_typedef env name = lookup env.typedefs name

let define_typedef env name vt =
  { env with typedefs = define env.typedefs name vt }

(* structs *)
let lookup_struct env tag = StringMap.find_opt tag env.globals.structs

let define_struct env tag fields =
  {
    env with
    globals =
      {
        env.globals with
        structs = StringMap.add tag fields env.globals.structs;
      };
  }

let define_func env name signature =
  {
    env with
    globals =
      {
        env.globals with
        funcs = StringMap.add name signature env.globals.funcs;
      };
  }

let with_globals env globals = { env with globals }
