open Ast

type func_sig = {
  params : typ list;
  ret : typ;
}

type env = {
  (* head is innermost scope. push when entering a new block, discard on exit *)
  vars : (string, typ) Hashtbl.t list;
  funcs : (string, func_sig) Hashtbl.t;
  typedefs : (string, var_type) Hashtbl.t list;
  return_typ : typ option;
  in_loop : bool;
}

let push_scope env =
  {
    env with
    vars = Hashtbl.create 8 :: env.vars;
    typedefs = Hashtbl.create 8 :: env.typedefs;
  }

let lookup_var env (x : string) =
  List.find_map (fun scope -> Hashtbl.find_opt scope x) env.vars

let lookup_typedef env (name : string) =
  List.find_map (fun scope -> Hashtbl.find_opt scope name) env.typedefs

(* let rec lookup_var2 (env : env) (e : 'a expr) : typ option = match e with |
   VarRef (_, name) -> lookup_var env name | UnaryOp (_, Deref, inner) ->
   lookup_var2 env inner | _ -> failwith "undefined variable" *)

let define_var env name t =
  match env.vars with
  | scope :: _ -> Hashtbl.replace scope name t
  | [] -> failwith "empty scope stack"

let define_typedef env name vt =
  match env.typedefs with
  | scope :: _ -> Hashtbl.replace scope name vt
  | [] -> failwith "empty typedef scope stack"
