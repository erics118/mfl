(** LLVM IR generation *)

open Ast

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context

(* builtin types *)
let void_type = Llvm.void_type context
let bool_type = Llvm.i1_type context

(* bool should be an i8 in memory, but for computation it is a i1 *)
let bool_mem_type = Llvm.i8_type context
let char_type = Llvm.i8_type context
let short_type = Llvm.i16_type context
let int_type = Llvm.i32_type context
let long_type = Llvm.i64_type context
let pointer_type = Llvm.pointer_type context

(** gets the size of a type in bytes *)
let rec sizeof_typ = function
  | Bool -> 1
  | Char | SChar | UChar -> 1
  | Short | UShort -> 2
  | Int | UInt -> 4
  | Long | ULong | LongLong | ULongLong -> 8
  | Ptr _ -> 8
  | Array (t, sz) -> sizeof_typ t * sz
  | Void -> 0

(* maps variable names to their alloca ptr within the current function *)
let locals : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

(* stack of (continue_bb, break_bb) for the each of the enclosing loops *)
let loop_stack : (Llvm.llbasicblock * Llvm.llbasicblock) Stack.t =
  Stack.create ()

(* true if the current block already ends with a terminator (ret, br, etc.) *)
let block_terminated () =
  Llvm.block_terminator (Llvm.insertion_block builder) <> None

(* emit an unconditional branch to bb only if the current block isn't already
   terminated *)
let br_if_open bb =
  if not (block_terminated ()) then ignore (Llvm.build_br bb builder)

(** zext i1 to i8 for storing a bool to memory *)
let bool_to_mem v name = Llvm.build_zext v bool_mem_type name builder

(** trunc i8 to i1 for loading a bool from memory *)
let bool_from_mem v name = Llvm.build_trunc v bool_type name builder

(* zext attribute for bool function params and ret values. zext i1 as necessary,
   matching the C ABI *)
let zext_attr = Llvm.create_enum_attr context "zeroext" 0L

(* convert a typechecker typ to an llvm type *)
let rec llvm_of_typ = function
  | Bool -> bool_mem_type
  | Void -> void_type
  | Char | SChar | UChar -> char_type
  | Short | UShort -> short_type
  | Int | UInt -> int_type
  | Long | ULong | LongLong | ULongLong -> long_type
  | Array (t, sz) -> Llvm.array_type (llvm_of_typ t) sz
  | Ptr _ -> pointer_type

(** load a value of type [t] from [ptr], returning the computation-ready value.
    handles any type-specific conversions, ie bool i8->i1 *)
let emit_load t ptr name =
  let v = Llvm.build_load (llvm_of_typ t) ptr name builder in
  if t = Bool then bool_from_mem v (name ^ "_b") else v

(** store a computation value of type [t] to [ptr]. handles any type-specific
    conversions, ie bool i1->i8 *)
let emit_store t v ptr =
  let v' = if t = Bool then bool_to_mem v "storeb" else v in
  ignore (Llvm.build_store v' ptr builder)

(* true for signed integer types *)
let is_signed = function
  | Char | SChar | Short | Int | Long | LongLong -> true
  | UChar | UShort | UInt | ULong | ULongLong -> false
  | Bool | Ptr _ | Array (_, _) | Void -> false

(* extract the resolved type from a checked expression annotation *)
let expr_type : checked expr -> typ = function
  | IntLiteral (Checked (_, t), _)
  | BoolLiteral (Checked (_, t), _)
  | CharLiteral (Checked (_, t), _)
  | VarRef (Checked (_, t), _)
  | BinaryOp (Checked (_, t), _, _, _)
  | UnaryOp (Checked (_, t), _, _)
  | Ternary (Checked (_, t), _, _, _)
  | FuncCall (Checked (_, t), _, _)
  | Assign (Checked (_, t), _, _)
  | PreInc (Checked (_, t), _)
  | PreDec (Checked (_, t), _)
  | PostInc (Checked (_, t), _)
  | PostDec (Checked (_, t), _)
  | Cast (Checked (_, t), _, _)
  | ImplicitCast (Checked (_, t), _, _)
  | Subscript (Checked (_, t), _, _) -> t
  | _ -> assert false [@coverage off]

let codegen_int n = Llvm.const_int int_type n

let rec codegen_binop operand_typ op lhs rhs =
  let lv = codegen_expr lhs in
  let rv = codegen_expr rhs in
  let signed = is_signed operand_typ in
  match op with
  | Add -> (
      match (expr_type lhs, expr_type rhs) with
      | Ptr t, _ ->
          (* ptr + int: ptr is lv, index is rv *)
          Llvm.build_gep (llvm_of_typ t) lv [| rv |] "addtmp" builder
      | _, Ptr t ->
          (* int + ptr: ptr is rv, index is lv *)
          Llvm.build_gep (llvm_of_typ t) rv [| lv |] "addtmp" builder
      | _ -> Llvm.build_add lv rv "addtmp" builder)
  | Sub -> (
      match (expr_type lhs, expr_type rhs) with
      | Ptr t, Ptr _ ->
          (* ptr - ptr: calculate ptrdiff with ptrtoint then sub *)
          let li = Llvm.build_ptrtoint lv long_type "ptrdiffl" builder in
          let ri = Llvm.build_ptrtoint rv long_type "ptrdiffr" builder in
          let diff = Llvm.build_sub li ri "ptrdiff" builder in
          (* we need to convert bits to bytes. use ceiling division. *)
          let size = Llvm.const_int long_type (sizeof_typ t) in
          Llvm.build_sdiv diff size "ptrdiff" builder
      | Ptr t, rhs_t ->
          (* ptr - int: first convert index to i64, negate, then gep. this way,
             we prevent wrap-around issues if offset is unsigned and greater
             than INT32_MAX *)
          let rv64 =
            if sizeof_typ rhs_t < 8 then
              if is_signed rhs_t then
                Llvm.build_sext rv long_type "sext" builder
              else Llvm.build_zext rv long_type "zext" builder
            else rv
          in
          let neg_rv = Llvm.build_neg rv64 "negidx" builder in
          Llvm.build_gep (llvm_of_typ t) lv [| neg_rv |] "subtmp" builder
      | _ -> Llvm.build_sub lv rv "subtmp" builder)
  | Mul -> Llvm.build_mul lv rv "multmp" builder
  | Div ->
      if signed then Llvm.build_sdiv lv rv "divtmp" builder
      else Llvm.build_udiv lv rv "divtmp" builder
  | Mod ->
      if signed then Llvm.build_srem lv rv "modtmp" builder
      else Llvm.build_urem lv rv "modtmp" builder
  | Equal -> Llvm.build_icmp Llvm.Icmp.Eq lv rv "eqtmp" builder
  | Neq -> Llvm.build_icmp Llvm.Icmp.Ne lv rv "neqtmp" builder
  | Less ->
      let pred = if signed then Llvm.Icmp.Slt else Llvm.Icmp.Ult in
      Llvm.build_icmp pred lv rv "lttmp" builder
  | Leq ->
      let pred = if signed then Llvm.Icmp.Sle else Llvm.Icmp.Ule in
      Llvm.build_icmp pred lv rv "letmp" builder
  | Greater ->
      let pred = if signed then Llvm.Icmp.Sgt else Llvm.Icmp.Ugt in
      Llvm.build_icmp pred lv rv "gttmp" builder
  | Geq ->
      let pred = if signed then Llvm.Icmp.Sge else Llvm.Icmp.Uge in
      Llvm.build_icmp pred lv rv "getmp" builder
  | And | Or ->
      (* implemented separately *)
      assert false [@coverage off]
  | BitAnd -> Llvm.build_and lv rv "bandtmp" builder
  | BitOr -> Llvm.build_or lv rv "bortmp" builder
  | BitXor -> Llvm.build_xor lv rv "xortmp" builder
  | LShift | RShift -> (
      let lv_ty = Llvm.type_of lv in
      let rv_ty = Llvm.type_of rv in
      (* zext right type as necessary, because lhs and rhs must have the same
         type *)
      let rv =
        if lv_ty = rv_ty then rv
        else
          let lv_bits = Llvm.integer_bitwidth lv_ty in
          let rv_bits = Llvm.integer_bitwidth rv_ty in
          if rv_bits < lv_bits then Llvm.build_zext rv lv_ty "shift_ext" builder
          else Llvm.build_trunc rv lv_ty "shift_trunc" builder
      in
      match op with
      | LShift -> Llvm.build_shl lv rv "shltmp" builder
      (* signed fills with sign bit, unsigned fills with zero *)
      | RShift ->
          if signed then Llvm.build_ashr lv rv "ashrtmp" builder
          else Llvm.build_lshr lv rv "lsrtmp" builder
      | _ -> assert false [@coverage off])

and codegen_uop op e =
  match op with
  | Neg ->
      let v = codegen_expr e in
      Llvm.build_neg v "" builder
  | Not ->
      let v = codegen_expr e in
      let zero = Llvm.const_null (Llvm.type_of v) in
      Llvm.build_icmp Llvm.Icmp.Eq v zero "nottmp" builder
  | Compl ->
      let v = codegen_expr e in
      Llvm.build_not v "compltmp" builder
  | AddrOf -> begin
      (* lvalues only *)
      match e with
      | VarRef (Checked _, name) -> Hashtbl.find locals name
      | UnaryOp (Checked _, Deref, ptr_e) -> codegen_expr ptr_e
      | _ -> assert false [@coverage off]
    end
  | Deref -> begin
      (* can only deref pointers *)
      match expr_type e with
      | Ptr t ->
          let ptr = codegen_expr e in
          emit_load t ptr "deref"
      | _ -> assert false [@coverage off]
    end

and codegen_func_call ret_t name args =
  let fn =
    match Llvm.lookup_function name the_module with
    | Some f -> f
    | None -> failwith ("undefined function: " ^ name)
  in
  let fn_ty = Llvm_ext.global_value_type fn in
  let arg_vals = Array.of_list (List.map codegen_expr args) in
  (* if the return type is void, the call instruction can't have a name *)
  let call_name = if ret_t = Void then "" else "calltmp" in
  let result = Llvm.build_call fn_ty fn arg_vals call_name builder in
  (* add zext attr to bool args and bool return at the call site *)
  if ret_t = Bool then
    Llvm.add_call_site_attr result zext_attr Llvm.AttrIndex.Return;
  List.iteri
    (fun i arg ->
      if expr_type arg = Bool then
        Llvm.add_call_site_attr result zext_attr (Llvm.AttrIndex.Param i))
    args;
  result

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

and codegen_expr (e : checked expr) : Llvm.llvalue =
  match e with
  | IntLiteral (Checked (_, t), n) -> Llvm.const_int (llvm_of_typ t) n
  | BoolLiteral (Checked _, b) ->
      (* bool literals produce i1 for computation *)
      Llvm.const_int bool_type (if b then 1 else 0)
  | CharLiteral (Checked _, c) -> Llvm.const_int char_type c
  | BinaryOp (Checked _, And, lhs, rhs) -> codegen_and_binop lhs rhs
  | BinaryOp (Checked _, Or, lhs, rhs) -> codegen_or_binop lhs rhs
  | BinaryOp (Checked _, op, lhs, rhs) ->
      codegen_binop (expr_type lhs) op lhs rhs
  | VarRef (Checked (_, t), name) -> begin
      (* load the value from the variable's alloca; type comes from the
         annotation *)
      match Hashtbl.find_opt locals name with
      | Some ptr -> begin
          (* arrays decay to a pointer to its first element *)
          match t with
          | Array (elem_t, _) ->
              let zero = Llvm.const_int int_type 0 in
              Llvm.build_gep (llvm_of_typ elem_t) ptr [| zero |] "arrdecay"
                builder
          | _ -> emit_load t ptr name
        end
      | None -> failwith ("undefined variable: " ^ name)
    end
  | UnaryOp (Checked _, op, e) -> codegen_uop op e
  | Ternary (Checked _, cond, then_e, else_e) ->
      codegen_ternary cond then_e else_e
  | FuncCall (Checked (_, ret_t), name, args) ->
      (* ret_t from the annotation determines whether the call can have a
         name *)
      codegen_func_call ret_t name args
  | Assign (Checked _, e, value) ->
      let ptr = lvalue_ptr e in
      let v = codegen_expr value in
      emit_store (expr_type value) v ptr;
      v (* return the computation value, not memory value *)
  | PreInc (Checked _, e) -> codegen_incdec e `Inc `Pre
  | PreDec (Checked _, e) -> codegen_incdec e `Dec `Pre
  | PostInc (Checked _, e) -> codegen_incdec e `Inc `Post
  | PostDec (Checked _, e) -> codegen_incdec e `Dec `Post
  | Subscript (Checked (_, elem_t), a, i) -> codegen_subscript elem_t a i
  | Cast (Checked _, t, e) -> codegen_cast (typ_of_var_type t) e
  | ImplicitCast (Checked _, t, e) -> codegen_cast t e
  | _ -> assert false [@coverage off]

(** gets the pointer to an element via subscript *)
and codegen_subscript elem_t a i =
  let ptr = array_base_ptr a in
  let iv = codegen_expr i in
  (* sext index to i64 for gep *)
  let iv64 =
    if Llvm.integer_bitwidth (Llvm.type_of iv) < 64 then
      Llvm.build_sext iv long_type "idxext" builder
    else iv
  in
  let elem_ptr =
    Llvm.build_gep (llvm_of_typ elem_t) ptr [| iv64 |] "subscript" builder
  in
  emit_load elem_t elem_ptr "subscriptval"

(* gets the base pointer for subscript. we can arrays use alloca ptr directly *)
and array_base_ptr (e : checked expr) : Llvm.llvalue =
  match expr_type e with
  | Array (_, _) -> (
      (* use alloca ptr directly, bypassing emit_load *)
      match e with
      | VarRef (_, name) -> Hashtbl.find locals name
      | _ -> assert false [@coverage off])
  | Ptr _ -> codegen_expr e
  | _ -> assert false [@coverage off]

(** gets the pointer to an lvalue *)
and lvalue_ptr (e : checked expr) : Llvm.llvalue =
  match e with
  | VarRef (_, name) -> Hashtbl.find locals name
  | UnaryOp (_, Deref, inner) -> codegen_expr inner
  | Subscript (Checked (_, elem_t), a, i) ->
      (* turn array into a pointer, then gep *)
      let ptr = array_base_ptr a in
      let iv = codegen_expr i in
      let iv64 =
        if Llvm.integer_bitwidth (Llvm.type_of iv) < 64 then
          Llvm.build_sext iv long_type "idxext" builder
        else iv
      in
      Llvm.build_gep (llvm_of_typ elem_t) ptr [| iv64 |] "subscript" builder
  | _ -> assert false [@coverage off]

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
  let ty = expr_type e in
  let ll_ty = llvm_of_typ ty in
  let ptr = lvalue_ptr e in
  (* load the memory value to edit directly. emit_load would trunc bool i8 to an
     i1, but arithmetic must happen on the i8 *)
  let old_val = Llvm.build_load ll_ty ptr "incdec" builder in
  (* update the new value by adding or subtracting 1 *)
  let new_val =
    match (dir, ty) with
    | `Inc, Ptr t ->
        (* we need to use gep 1 for pointers *)
        let one = Llvm.const_int int_type 1 in
        Llvm.build_gep (llvm_of_typ t) old_val [| one |] "incdec" builder
    | `Dec, Ptr t ->
        (* we need to use gep -1 for pointers *)
        let neg_one = Llvm.const_int int_type (-1) in
        Llvm.build_gep (llvm_of_typ t) old_val [| neg_one |] "incdec" builder
    | `Inc, _ ->
        (* add 1 *)
        let one = Llvm.const_int ll_ty 1 in
        Llvm.build_add old_val one "incdec" builder
    | `Dec, _ ->
        (* subtract 1 *)
        let one = Llvm.const_int ll_ty 1 in
        Llvm.build_sub old_val one "incdec" builder
  in
  (* normalize bool to 0 or 1. non-zero becomes 1, zero stays 0 *)
  let new_val =
    if ty = Bool then
      let zero = Llvm.const_null bool_mem_type in
      let b = Llvm.build_icmp Llvm.Icmp.Ne new_val zero "boolnorm" builder in
      bool_to_mem b "boolnorm_mem"
    else new_val
  in
  (* update the value *)
  ignore (Llvm.build_store new_val ptr builder);
  (* if postfix, return the old value; if prefix, return the new value *)
  let result =
    match fix with
    | `Post -> old_val
    | `Pre -> new_val
  in
  (* bool is stored as i8; trunc back to i1 for computation *)
  if ty = Bool then bool_from_mem result "incdecb" else result

and codegen_cast to_t e =
  let v = codegen_expr e in
  let from_t = expr_type e in
  if from_t = to_t then
    (* already the same type, we don't need to do anything *)
    v
  else
    (* different types, we need to convert *)
    let to_ll = llvm_of_typ to_t in
    match (from_t, to_t) with
    | Ptr _, Ptr _ ->
        (* cast from pointer type to another pointer type *)
        Llvm.build_pointercast v to_ll "ptrcasttmp" builder
    | Ptr _, _ ->
        (* explicit cast from pointer to integer *)
        Llvm.build_ptrtoint v to_ll "ptrtointtmp" builder
    | _, Ptr _ ->
        (* explicit cast from integer to pointer *)
        Llvm.build_inttoptr v to_ll "inttoptrtmp" builder
    | _, Bool ->
        (* we have to explicitly handle bool bc trunc may not necessarily work.
           ie, trunc i32 2 to i1 gives 0 not 1. it should be that any nonzero
           value is true. instead, we do a != 0 comparison *)
        let zero = Llvm.const_null (llvm_of_typ from_t) in
        Llvm.build_icmp Llvm.Icmp.Ne v zero "booltmp" builder
    | _ ->
        (* non-bool integer case *)
        let from_size = sizeof_typ from_t in
        let to_size = sizeof_typ to_t in
        if to_size < from_size then
          (* if we need to make the value smaller, we trunc *)
          Llvm.build_trunc v to_ll "trunctmp" builder
        else if to_size > from_size then
          (* if we need to make the value larger, we ext, handling sign *)
          if is_signed from_t then Llvm.build_sext v to_ll "sexttmp" builder
          else Llvm.build_zext v to_ll "zexttmp" builder
        else
          (* same size, different signedness, don't do anything *)
          v

and codegen_func_def ret_type name params body =
  (* use i1 for bool in signatures, not i8. zeroext handles the ABI *)
  let llvm_of_sig_typ t = if t = Bool then bool_type else llvm_of_typ t in
  let param_types =
    Array.of_list
      (List.map
         (fun (vt, _) -> llvm_of_sig_typ (Ast.typ_of_var_type vt))
         params)
  in
  let ret_t = Ast.typ_of_var_type ret_type in
  let ty = Llvm.function_type (llvm_of_sig_typ ret_t) param_types in
  let fn = Llvm.define_function name ty the_module in
  (* add zeroext to bool return and bool parameters *)
  if ret_t = Bool then Llvm.add_function_attr fn zext_attr Llvm.AttrIndex.Return;
  List.iteri
    (fun i (vt, _) ->
      if Ast.typ_of_var_type vt = Bool then
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
      let t = Ast.typ_of_var_type ptype in
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

and codegen_stmt = function
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
  | VarDef { var_type; name; init; _ } ->
      let ty = llvm_of_typ (Ast.typ_of_var_type var_type) in
      let ptr = Llvm.build_alloca ty name builder in
      (* set variable to init if it exists *)
      begin match init with
      | None -> ()
      | Some init ->
          let v = codegen_expr init in
          emit_store (Ast.typ_of_var_type var_type) v ptr
      end;
      Hashtbl.replace locals name ptr
  | If { cond; then_body; else_body; _ } -> codegen_if cond then_body else_body
  | WhileLoop { cond; body; _ } -> codegen_while_loop cond body
  | ForLoop { init; cond; incr; body; _ } ->
      codegen_for_loop init cond incr body
  | DoWhileLoop { body; cond; _ } -> codegen_do_while_loop body cond

let codegen_program (stmts : checked stmt list) : unit =
  let printint_ty =
    Llvm.function_type (Llvm.void_type context) [| int_type |]
  in
  ignore (Llvm.declare_function "printint" printint_ty the_module);
  let printbool_ty =
    Llvm.function_type (Llvm.void_type context) [| bool_type |]
  in
  let printbool_fn =
    Llvm.declare_function "printbool" printbool_ty the_module
  in
  Llvm.add_function_attr printbool_fn zext_attr (Llvm.AttrIndex.Param 0);
  List.iter codegen_stmt stmts

let emit_ir () = Llvm.string_of_llmodule the_module [@coverage off]
