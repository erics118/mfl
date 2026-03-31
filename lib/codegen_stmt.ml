open Ast
open Codegen_context
open Codegen_expr

let builtin_names = [ "printint"; "printbool"; "malloc" ]
let is_builtin name = List.mem name builtin_names

let rec collect_builtin_expr acc = function
  | IntLiteral _ | BoolLiteral _ | CharLiteral _ | VarRef _ | SizeofType _ ->
      acc
  | UnaryOp (_, _, e)
  | PreInc (_, e)
  | PreDec (_, e)
  | PostInc (_, e)
  | PostDec (_, e)
  | Cast (_, _, e)
  | ImplicitCast (_, _, e)
  | SizeofExpr (_, e) -> collect_builtin_expr acc e
  | BinaryOp (_, _, lhs, rhs) | Assign (_, lhs, rhs) | Subscript (_, lhs, rhs)
    ->
      let acc = collect_builtin_expr acc lhs in
      collect_builtin_expr acc rhs
  | Ternary (_, c, t, e) ->
      let acc = collect_builtin_expr acc c in
      let acc = collect_builtin_expr acc t in
      collect_builtin_expr acc e
  | MemberAccess (_, e, _) -> collect_builtin_expr acc e
  | FuncCall (_, name, args) ->
      let acc = if is_builtin name then name :: acc else acc in
      List.fold_left collect_builtin_expr acc args

let rec collect_builtin_stmt acc = function
  | ExprStmt (_, e) -> collect_builtin_expr acc e
  | ReturnStmt (_, None) | BreakStmt _ | ContinueStmt _ | EmptyStmt _ -> acc
  | ReturnStmt (_, Some e) | VarDef { init = Some e; _ } ->
      collect_builtin_expr acc e
  | VarDef { init = None; _ } | Typedef _ | StructDef _ -> acc
  | CompoundStmt (_, stmts) -> List.fold_left collect_builtin_stmt acc stmts
  | FuncDef { body; _ } -> List.fold_left collect_builtin_stmt acc body
  | If { cond; then_body; else_body; _ } ->
      let acc = collect_builtin_expr acc cond in
      let acc = collect_builtin_stmt acc then_body in
      Option.fold ~none:acc ~some:(collect_builtin_stmt acc) else_body
  | WhileLoop { cond; body; _ } | DoWhileLoop { cond; body; _ } ->
      let acc = collect_builtin_expr acc cond in
      collect_builtin_stmt acc body
  | ForLoop { init; cond; incr; body; _ } ->
      let acc = collect_builtin_stmt acc init in
      let acc = Option.fold ~none:acc ~some:(collect_builtin_expr acc) cond in
      let acc = Option.fold ~none:acc ~some:(collect_builtin_expr acc) incr in
      collect_builtin_stmt acc body

let rec codegen_if cond then_body else_body =
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
  Stack.push (cond_bb, after_bb) loop_stack;
  codegen_stmt body;
  ignore (Stack.pop loop_stack);
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
  Stack.push (incr_bb, after_bb) loop_stack;
  codegen_stmt body;
  ignore (Stack.pop loop_stack);
  (* jump back to incr *)
  br_if_open incr_bb;
  (* run the incr *)
  Llvm.position_at_end incr_bb builder;
  ignore (Option.map codegen_expr incr);
  (* jump back to cond *)
  br_if_open cond_bb;
  Llvm.position_at_end after_bb builder

and codegen_do_while_loop body cond =
  let fn = Llvm.block_parent (Llvm.insertion_block builder) in
  let body_bb = Llvm.append_block context "do_body" fn in
  let cond_bb = Llvm.append_block context "do_cond" fn in
  let after_bb = Llvm.append_block context "do_after" fn in
  (* fall into the body unconditionally *)
  ignore (Llvm.build_br body_bb builder);
  (* run the body, then jump to cond *)
  Llvm.position_at_end body_bb builder;
  Stack.push (cond_bb, after_bb) loop_stack;
  codegen_stmt body;
  ignore (Stack.pop loop_stack);
  br_if_open cond_bb;
  (* evaluate cond; loop back or exit *)
  Llvm.position_at_end cond_bb builder;
  let c = codegen_expr cond in
  ignore (Llvm.build_cond_br c body_bb after_bb builder);
  Llvm.position_at_end after_bb builder

and codegen_func_def ret_type name params body =
  (* use i1 for bool in signatures, not i8. zeroext handles the ABI *)
  let llvm_of_sig_typ t = if t = Bool then bool_type else llvm_of_typ t in
  let param_types =
    Array.of_list
      (List.map
         (fun (vt, _) -> llvm_of_sig_typ (Ast.typ_of_source_type vt))
         params)
  in
  let ret_t = Ast.typ_of_source_type ret_type in
  let ty = Llvm.function_type (llvm_of_sig_typ ret_t) param_types in
  let fn = Llvm.define_function name ty the_module in
  (* add zeroext to bool return and bool parameters *)
  if ret_t = Bool then Llvm.add_function_attr fn zext_attr Llvm.AttrIndex.Return;
  List.iteri
    (fun i (vt, _) ->
      if Ast.typ_of_source_type vt = Bool then
        Llvm.add_function_attr fn zext_attr (Llvm.AttrIndex.Param i))
    params;
  (* clear scope for each function. no global variables atm *)
  Hashtbl.clear locals;
  (* allocate each param on the stack and store the incoming value, so params
     can be treated as mutable locals *)
  List.iteri
    (fun i (ptype, pname) ->
      let param = Llvm.param fn i in
      Llvm.set_value_name pname param;
      Llvm.position_at_end (Llvm.entry_block fn) builder;
      let t = Ast.typ_of_source_type ptype in
      let ptr = Llvm.build_alloca (llvm_of_typ t) pname builder in
      emit_store t param ptr;
      Hashtbl.replace locals pname ptr)
    params;
  Llvm.position_at_end (Llvm.entry_block fn) builder;
  List.iter codegen_stmt body;
  if not (block_terminated ()) then
    if Llvm.return_type ty = Llvm.void_type context then
      ignore (Llvm.build_ret_void builder)
    else if name = "main" then
      (* add return 0; to main if missing *)
      ignore (Llvm.build_ret (Llvm.const_null (Llvm.return_type ty)) builder)

and codegen_struct_def tag fields =
  (* keep the synthetic tag internal *)
  let llty =
    Llvm.named_struct_type context ("struct." ^ Ast.display_struct_tag tag)
  in
  let field_types =
    Array.of_list (List.map (fun (_, ft) -> llvm_of_typ ft) fields)
  in
  Llvm.struct_set_body llty field_types false;
  Hashtbl.replace struct_defs tag (llty, fields)

and codegen_stmt = function
  | StructDef { tag; fields; _ } ->
      let resolved =
        List.map (fun (vt, fname) -> (fname, Ast.typ_of_source_type vt)) fields
      in
      codegen_struct_def tag resolved
  | FuncDef { ret_type; name; params; body; _ } ->
      codegen_func_def ret_type name params body
  | ReturnStmt (_, Some e) ->
      let v = codegen_expr e in
      ignore (Llvm.build_ret v builder)
  | ReturnStmt (_, None) -> ignore (Llvm.build_ret_void builder)
  | BreakStmt _ ->
      let _, break_bb = Stack.top loop_stack in
      ignore (Llvm.build_br break_bb builder)
  | ContinueStmt _ ->
      let continue_bb, _ = Stack.top loop_stack in
      ignore (Llvm.build_br continue_bb builder)
  | ExprStmt (_, e) -> ignore (codegen_expr e)
  | EmptyStmt _ -> ()
  | CompoundStmt (_, stmts) -> List.iter codegen_stmt stmts
  | Typedef { struct_def; _ } -> begin
      (* register inline struct definition if present *)
      match struct_def with
      | None -> ()
      | Some (tag, fields) ->
          let resolved =
            List.map
              (fun (vt, fname) -> (fname, Ast.typ_of_source_type vt))
              fields
          in
          codegen_struct_def tag resolved
    end
  | VarDef { source_type; name; init; _ } ->
      let ty = llvm_of_typ (Ast.typ_of_source_type source_type) in
      let ptr = Llvm.build_alloca ty name builder in
      (* set variable to init if it exists *)
      begin match init with
      | None -> ()
      | Some init ->
          let v = codegen_expr init in
          emit_store (Ast.typ_of_source_type source_type) v ptr
      end;
      Hashtbl.replace locals name ptr
  | If { cond; then_body; else_body; _ } -> codegen_if cond then_body else_body
  | WhileLoop { cond; body; _ } -> codegen_while_loop cond body
  | ForLoop { init; cond; incr; body; _ } ->
      codegen_for_loop init cond incr body
  | DoWhileLoop { body; cond; _ } -> codegen_do_while_loop body cond

let codegen_program (stmts : checked stmt list) : unit =
  let builtins =
    List.fold_left collect_builtin_stmt [] stmts
    |> List.sort_uniq String.compare
  in
  List.iter (fun name -> ignore (ensure_builtin_decl name)) builtins;
  List.iter codegen_stmt stmts

let emit_ir () = Llvm.string_of_llmodule the_module [@coverage off]
