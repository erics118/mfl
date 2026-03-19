let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context
let int_type = Llvm.i32_type context
let bool_type = Llvm.i1_type context

(* maps variable names to their alloca ptr within the current function *)
let locals : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

(* true if the current block already ends with a terminator (ret, br, etc.) *)
let block_terminated () =
  Llvm.block_terminator (Llvm.insertion_block builder) <> None

(* emit an unconditional branch to bb only if the current block isn't already
   terminated *)
let br_if_open bb =
  if not (block_terminated ()) then ignore (Llvm.build_br bb builder)

let llvm_type = function
  | Ast.VarType "int" -> int_type
  | Ast.VarType "bool" -> bool_type
  | Ast.VarType "void" -> Llvm.void_type context
  | Ast.VarType name -> failwith ("unknown type: " ^ name)

(* convert a typechecker typ to an llvm type *)
let llvm_of_typ = function
  | Ast.Int -> int_type
  | Ast.Bool -> bool_type
  | Ast.Void -> Llvm.void_type context

let codegen_int n = Llvm.const_int int_type n

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
  | Ast.And | Ast.Or ->
      assert false (* implemented separately *) [@coverage off]
  | Ast.BitAnd -> Llvm.build_and lv rv "bandtmp" builder
  | Ast.BitOr -> Llvm.build_or lv rv "bortmp" builder
  | Ast.BitXor -> Llvm.build_xor lv rv "xortmp" builder
  | Ast.LShift -> Llvm.build_shl lv rv "shltmp" builder
  | Ast.RShift -> Llvm.build_ashr lv rv "ashrtmp" builder

and codegen_uop op e =
  let v = codegen_expr e in
  match op with
  | Ast.Neg -> Llvm.build_neg v "" builder
  | Ast.Not -> Llvm.build_not v "nottmp" builder
  | Ast.Compl -> Llvm.build_not v "compltmp" builder

and codegen_func_call ret_t name args =
  let fn =
    match Llvm.lookup_function name the_module with
    | Some f -> f
    | None -> failwith ("undefined function: " ^ name)
  in
  let fn_ty = Llvm_ext.global_value_type fn in
  let arg_vals = Array.of_list (List.map codegen_expr args) in
  (* if the return type is void, the call instruction can't have a name *)
  let call_name = if ret_t = Ast.Void then "" else "calltmp" in
  Llvm.build_call fn_ty fn arg_vals call_name builder

and codegen_and_binop lhs rhs =
  let lv = codegen_expr lhs in
  let lhs_bb = Llvm.insertion_block builder in
  let fn = Llvm.block_parent lhs_bb in
  let rhs_bb = Llvm.append_block context "and_rhs" fn in
  let merge_bb = Llvm.append_block context "and_merge" fn in
  (* if lhs is false, jump to merge. else evaluate rhs *)
  ignore (Llvm.build_cond_br lv rhs_bb merge_bb builder);
  Llvm.position_at_end rhs_bb builder;
  let rv = codegen_expr rhs in
  let rhs_bb' = Llvm.insertion_block builder in
  ignore (Llvm.build_br merge_bb builder);
  (* select either lhs or rhs *)
  Llvm.position_at_end merge_bb builder;
  Llvm.build_phi [ (lv, lhs_bb); (rv, rhs_bb') ] "andtmp" builder

and codegen_or_binop lhs rhs =
  let lv = codegen_expr lhs in
  let lhs_bb = Llvm.insertion_block builder in
  let fn = Llvm.block_parent lhs_bb in
  let rhs_bb = Llvm.append_block context "or_rhs" fn in
  let merge_bb = Llvm.append_block context "or_merge" fn in
  (* if lhs is true, jump to merge. evaluate rhs *)
  ignore (Llvm.build_cond_br lv merge_bb rhs_bb builder);
  Llvm.position_at_end rhs_bb builder;
  let rv = codegen_expr rhs in
  let rhs_bb' = Llvm.insertion_block builder in
  ignore (Llvm.build_br merge_bb builder);
  (* select either lhs or rhs *)
  Llvm.position_at_end merge_bb builder;
  Llvm.build_phi [ (lv, lhs_bb); (rv, rhs_bb') ] "ortmp" builder

and codegen_expr (e : Ast.checked Ast.expr) : Llvm.llvalue =
  match e with
  | Ast.IntLiteral (_, n) -> codegen_int n
  | Ast.BoolLiteral (_, b) -> Llvm.const_int bool_type (if b then 1 else 0)
  | Ast.BinaryOp (_, Ast.And, lhs, rhs) -> codegen_and_binop lhs rhs
  | Ast.BinaryOp (_, Ast.Or, lhs, rhs) -> codegen_or_binop lhs rhs
  | Ast.BinaryOp (_, op, lhs, rhs) -> codegen_binop op lhs rhs
  | Ast.VarRef (Ast.Checked (_, t), name) -> (
      (* load the value from the variable's alloca; type comes from the
         annotation *)
      match Hashtbl.find_opt locals name with
      | Some ptr -> Llvm.build_load (llvm_of_typ t) ptr name builder
      | None -> failwith ("undefined variable: " ^ name))
  | Ast.VarRef (Ast.Parsed _, _) -> assert false
  | Ast.UnaryOp (_, op, e) -> codegen_uop op e
  | Ast.Ternary (_, cond, then_e, else_e) -> codegen_ternary cond then_e else_e
  | Ast.FuncCall (Ast.Checked (_, ret_t), name, args) ->
      (* ret_t from the annotation determines whether the call can have a
         name *)
      codegen_func_call ret_t name args
  | Ast.FuncCall (Ast.Parsed _, _, _) -> assert false
  | Ast.Assign (_, name, value) -> (
      match Hashtbl.find_opt locals name with
      | Some ptr ->
          (* codegen, then return the assigned value, so we can do x = y = 2 *)
          let v = codegen_expr value in
          ignore (Llvm.build_store v ptr builder);
          v
      | None -> failwith ("undefined variable: " ^ name))
  | PreInc (_, e) -> codegen_incdec e `Inc `Pre
  | PreDec (_, e) -> codegen_incdec e `Dec `Pre
  | PostInc (_, e) -> codegen_incdec e `Inc `Post
  | PostDec (_, e) -> codegen_incdec e `Dec `Post

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

and codegen_while_loop cond body =
  let fn = Llvm.block_parent (Llvm.insertion_block builder) in
  let cond_bb = Llvm.append_block context "while_cond" fn in
  let body_bb = Llvm.append_block context "while_body" fn in
  let after_bb = Llvm.append_block context "while_after" fn in
  (* go into cond *)
  ignore (Llvm.build_br cond_bb builder);
  (* evaluate cond, then branch *)
  Llvm.position_at_end cond_bb builder;
  let c = codegen_expr cond in
  ignore (Llvm.build_cond_br c body_bb after_bb builder);
  (* run the body, then jump back to cond *)
  Llvm.position_at_end body_bb builder;
  codegen_stmt body;
  br_if_open cond_bb;
  Llvm.position_at_end after_bb builder

and codegen_for_loop init cond incr body =
  let fn = Llvm.block_parent (Llvm.insertion_block builder) in
  let init_bb = Llvm.append_block context "for_init" fn in
  let cond_bb = Llvm.append_block context "for_cond" fn in
  let incr_bb = Llvm.append_block context "for_incr" fn in
  let body_bb = Llvm.append_block context "for_body" fn in
  let after_bb = Llvm.append_block context "for_after" fn in
  (* execute init first *)
  ignore (Llvm.build_br init_bb builder);
  Llvm.position_at_end init_bb builder;
  codegen_stmt init;

  (* go into cond *)
  ignore (Llvm.build_br cond_bb builder);
  (* evaluate cond, then branch *)
  Llvm.position_at_end cond_bb builder;
  begin match cond with
  | None ->
      (* go to body_bb *)
      ignore (Llvm.build_br body_bb builder)
  | Some cond -> begin
      (* generate the code for cond *)
      let c = codegen_expr cond in
      (* if c is true, go to body_bb, otherwise go to after_bb *)
      ignore (Llvm.build_cond_br c body_bb after_bb builder)
    end
  end;
  (* run the body *)
  Llvm.position_at_end body_bb builder;
  codegen_stmt body;
  (* jump back to incr *)
  br_if_open incr_bb;
  (* run the incr *)
  Llvm.position_at_end incr_bb builder;
  ignore (Option.map codegen_expr incr);
  (* jump back to cond *)
  br_if_open cond_bb;
  Llvm.position_at_end after_bb builder

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

and codegen_incdec e dir fix =
  let delta =
    match dir with
    | `Inc -> 1
    | `Dec -> -1
  in
  let name =
    match e with
    | Ast.VarRef (_, n) -> n
    | _ ->
        (* we know it is a lvalue, ie a VarRef. this will have to change in the
           future though, after adding pointers, arrays, etc *)
        assert false
  in
  let ptr = Hashtbl.find locals name in
  (* get the old value *)
  let old_val = Llvm.build_load int_type ptr name builder in
  (* update the new value with by adding 1 or -1 *)
  let new_val =
    Llvm.build_add old_val (Llvm.const_int int_type delta) "incdec" builder
  in
  (* update the value *)
  ignore (Llvm.build_store new_val ptr builder);
  (* if postfix, return the old value; if prefix, return the new value *)
  match fix with
  | `Post -> old_val
  | `Pre -> new_val

and codegen_func_def ret_type name params body =
  let param_types =
    Array.of_list (List.map (fun (vt, _) -> llvm_type vt) params)
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
      Hashtbl.replace locals pname ptr)
    params;
  Llvm.position_at_end (Llvm.entry_block fn) builder;
  List.iter codegen_stmt body;
  if not (block_terminated ()) then
    if Llvm.return_type ty = Llvm.void_type context then
      ignore (Llvm.build_ret_void builder)
    else if name = "main" then
      (* add return 0; to main if missing *)
      ignore (Llvm.build_ret (Llvm.const_null (llvm_type ret_type)) builder)

and codegen_stmt = function
  | Ast.FuncDef { ret_type; name; params; body; _ } ->
      codegen_func_def ret_type name params body
  | Ast.ReturnStmt (_, Some e) ->
      ignore (Llvm.build_ret (codegen_expr e) builder)
  | Ast.ReturnStmt (_, None) -> ignore (Llvm.build_ret_void builder)
  | Ast.ExprStmt (_, e) -> ignore (codegen_expr e)
  | Ast.EmptyStmt _ -> ()
  | Ast.CompoundStmt (_, stmts) -> List.iter codegen_stmt stmts
  | Ast.VarDef { var_type; name; init; _ } ->
      let ty = llvm_type var_type in
      let ptr = Llvm.build_alloca ty name builder in
      (* set variable to init if it exists *)
      begin match init with
      | None -> ()
      | Some init -> ignore (Llvm.build_store (codegen_expr init) ptr builder)
      end;
      Hashtbl.replace locals name ptr
  | Ast.If { cond; then_body; else_body; _ } ->
      codegen_if cond then_body else_body
  | Ast.WhileLoop { cond; body; _ } -> codegen_while_loop cond body
  | Ast.ForLoop { init; cond; incr; body; _ } ->
      codegen_for_loop init cond incr body

let codegen_program stmts =
  let printint_ty =
    Llvm.function_type (Llvm.void_type context) [| int_type |]
  in
  ignore (Llvm.declare_function "printint" printint_ty the_module);
  let printbool_ty =
    Llvm.function_type (Llvm.void_type context) [| bool_type |]
  in
  ignore (Llvm.declare_function "printbool" printbool_ty the_module);
  List.iter codegen_stmt stmts

let emit_ir () = Llvm.string_of_llmodule the_module [@coverage off]
