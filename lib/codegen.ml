let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"

let () =
  Llvm.set_target_triple (Llvm_target.Target.default_triple ()) the_module

let builder = Llvm.builder context
let int_type = Llvm.i32_type context
let bool_type = Llvm.i1_type context

(* maps variable names to (type, alloca ptr) within the current function *)
let locals : (string, Llvm.lltype * Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

(* when true, emit integer constants directly instead of alloca/store/load *)
let ssa = true

let codegen_int n =
  if ssa then Llvm.const_int int_type n
  else
    let ptr = Llvm.build_alloca int_type "numptr" builder in
    ignore (Llvm.build_store (Llvm.const_int int_type n) ptr builder);
    Llvm.build_load int_type ptr "num" builder

let rec codegen_binop op lhs rhs =
  let lv = codegen_expr lhs in
  let rv = codegen_expr rhs in
  match op with
  | Ast.Add -> Llvm.build_add lv rv "addtmp" builder
  | Ast.Sub -> Llvm.build_sub lv rv "subtmp" builder
  | Ast.Mul -> Llvm.build_mul lv rv "multmp" builder
  | Ast.Div -> Llvm.build_sdiv lv rv "divtmp" builder
  | Ast.Mod -> Llvm.build_srem lv rv "modtmp" builder
  | Ast.Equal -> Llvm.build_icmp Llvm.Icmp.Eq lv rv "eqtmp" builder
  | Ast.Neq -> Llvm.build_icmp Llvm.Icmp.Ne lv rv "neqtmp" builder
  | Ast.Less -> Llvm.build_icmp Llvm.Icmp.Slt lv rv "lttmp" builder
  | Ast.Leq -> Llvm.build_icmp Llvm.Icmp.Sle lv rv "letmp" builder
  | Ast.Greater -> Llvm.build_icmp Llvm.Icmp.Sgt lv rv "gttmp" builder
  | Ast.Geq -> Llvm.build_icmp Llvm.Icmp.Sge lv rv "getmp" builder
  | Ast.And -> Llvm.build_and lv rv "andtmp" builder
  | Ast.Or -> Llvm.build_or lv rv "ortmp" builder
  | Ast.BitAnd -> Llvm.build_and lv rv "bandtmp" builder
  | Ast.BitOr -> Llvm.build_or lv rv "bortmp" builder
  | Ast.BitXor -> Llvm.build_xor lv rv "xortmp" builder

and codegen_uop op expr =
  let v = codegen_expr expr in
  match op with
  | Ast.Neg -> Llvm.build_neg v "" builder
  | Ast.Not -> Llvm.build_not v "nottmp" builder

and codegen_func_call _name _args = failwith ""

and codegen_expr = function
  | Ast.IntLiteral n -> codegen_int n
  | Ast.BoolLiteral b -> Llvm.const_int bool_type (if b then 1 else 0)
  | Ast.BinaryOp (op, lhs, rhs) -> codegen_binop op lhs rhs
  | Ast.VarRef name -> (
      (* load the value from the variable's alloca *)
      match Hashtbl.find_opt locals name with
      | Some (ty, ptr) -> Llvm.build_load ty ptr name builder
      | None -> failwith ("undefined variable: " ^ name))
  | UnaryOp (op, expr) -> codegen_uop op expr
  | Ternary (cond, then_e, else_e) -> codegen_ternary cond then_e else_e
  | FuncCall { name; args } -> codegen_func_call name args

(* true if the current block already ends with a terminator (ret, br, etc.) *)
and block_terminated () =
  Llvm.block_terminator (Llvm.insertion_block builder) <> None

(* emit an unconditional branch to bb only if the current block isn't already
   terminated *)
and br_if_open bb =
  if not (block_terminated ()) then ignore (Llvm.build_br bb builder)

and codegen_if cond then_body else_body =
  let c = codegen_expr cond in
  let fn = Llvm.block_parent (Llvm.insertion_block builder) in
  (* create three empty blocks: one for each branch and one to continue after *)
  let then_bb = Llvm.append_block context "then" fn in
  let else_bb = Llvm.append_block context "else" fn in
  let merge_bb = Llvm.append_block context "merge" fn in
  (* terminate the current block by branching to then or else *)
  ignore (Llvm.build_cond_br c then_bb else_bb builder);
  (* emit the then branch; jump to merge unless it already returned *)
  Llvm.position_at_end then_bb builder;
  codegen_stmt then_body;
  br_if_open merge_bb;
  (* emit the else branch (or nothing). jump to merge unless it already
     returned *)
  Llvm.position_at_end else_bb builder;
  (match else_body with
  | Some s -> codegen_stmt s
  | None -> ());
  br_if_open merge_bb;
  (* all future codegen continues in the merge block *)
  Llvm.position_at_end merge_bb builder

and codegen_ternary cond then_e else_e =
  let c = codegen_expr cond in
  let fn = Llvm.block_parent (Llvm.insertion_block builder) in
  (* codegen then branch *)
  let then_bb = Llvm.append_block context "ternary_then" fn in
  (* codegen else branch *)
  let else_bb = Llvm.append_block context "ternary_else" fn in
  let merge_bb = Llvm.append_block context "ternary_merge" fn in
  ignore (Llvm.build_cond_br c then_bb else_bb builder);
  (* may create extra branches from nested, so we need to merge *)
  Llvm.position_at_end then_bb builder;
  let tv = codegen_expr then_e in
  let then_bb' = Llvm.insertion_block builder in
  ignore (Llvm.build_br merge_bb builder);
  Llvm.position_at_end else_bb builder;
  let ev = codegen_expr else_e in
  let else_bb' = Llvm.insertion_block builder in
  ignore (Llvm.build_br merge_bb builder);
  Llvm.position_at_end merge_bb builder;
  (* phi, to select value from whichever branch was taken *)
  Llvm.build_phi [ (tv, then_bb'); (ev, else_bb') ] "ternary" builder

and llvm_type = function
  | Ast.VarType "int" -> int_type
  | Ast.VarType "bool" -> bool_type
  | Ast.VarType t -> failwith ("unknown type: " ^ t)

and codegen_func ret_type name params body =
  let param_types =
    Array.of_list (List.map (fun (t, _) -> llvm_type t) params)
  in
  let ty = Llvm.function_type (llvm_type ret_type) param_types in
  let fn = Llvm.define_function name ty the_module in
  (* clear scope for each function. no global variables atm *)
  Hashtbl.clear locals;
  (* allocate each param on the stack and store the incoming value, so params
     can be treated as mutable locals *)
  List.iteri
    (fun i (ptype, pname) ->
      let param = Llvm.param fn i in
      Llvm.set_value_name pname param;
      Llvm.position_at_end (Llvm.entry_block fn) builder;
      let ty = llvm_type ptype in
      let ptr = Llvm.build_alloca ty pname builder in
      ignore (Llvm.build_store param ptr builder);
      Hashtbl.replace locals pname (ty, ptr))
    params;
  Llvm.position_at_end (Llvm.entry_block fn) builder;
  List.iter codegen_stmt body;
  (* add return 0; to main if missing *)
  if (not (block_terminated ())) && name = "main" then
    ignore (Llvm.build_ret (Llvm.const_null (llvm_type ret_type)) builder)

and codegen_stmt = function
  | Ast.FuncDef { ret_type; name; params; body } ->
      codegen_func ret_type name params body
  | Ast.ReturnStmt (Some e) -> ignore (Llvm.build_ret (codegen_expr e) builder)
  | Ast.ReturnStmt None -> ignore (Llvm.build_ret_void builder)
  | Ast.ExprStmt e -> ignore (codegen_expr e)
  | Ast.EmptyStmt -> ()
  | Ast.CompoundStmt stmts -> List.iter codegen_stmt stmts
  | Ast.VarDef { var_type; name; init } ->
      let ty = llvm_type var_type in
      let ptr = Llvm.build_alloca ty name builder in
      ignore (Llvm.build_store (codegen_expr init) ptr builder);
      Hashtbl.replace locals name (ty, ptr)
  | Ast.AssignStmt { name; value } -> (
      match Hashtbl.find_opt locals name with
      | Some (_, ptr) ->
          ignore (Llvm.build_store (codegen_expr value) ptr builder)
      | None -> failwith ("undefined variable: " ^ name))
  | Ast.If { cond; then_body; else_body } -> codegen_if cond then_body else_body
  | Ast.WhileLoop _ -> failwith "todo"
  | Ast.ForLoop _ -> failwith "todo"

let codegen_program stmts =
  ignore
    (Llvm.declare_function "printint"
       (Llvm.function_type (Llvm.void_type context) [| int_type |])
       the_module);
  ignore
    (Llvm.declare_function "printbool"
       (Llvm.function_type (Llvm.void_type context) [| bool_type |])
       the_module);
  List.iter codegen_stmt stmts

let emit_ir () = Llvm.string_of_llmodule the_module
