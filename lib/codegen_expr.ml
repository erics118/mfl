open Ast
open Codegen_context
open Typechecker_types

let rec codegen_binop operand_typ op lhs rhs =
  match operand_typ with
  | Float | Double | LongDouble -> codegen_float_binop op lhs rhs
  | _ -> codegen_int_and_ptr_binop operand_typ op lhs rhs

and codegen_float_binop op lhs rhs =
  let lv = codegen_expr lhs in
  let rv = codegen_expr rhs in
  match op with
  | Add -> Llvm.build_fadd lv rv "addtmp" builder
  | Sub -> Llvm.build_fsub lv rv "subtmp" builder
  | Mul -> Llvm.build_fmul lv rv "multmp" builder
  | Div -> Llvm.build_fdiv lv rv "divtmp" builder
  | Equal -> Llvm.build_fcmp Llvm.Fcmp.Oeq lv rv "eqtmp" builder
  | Neq ->
      (* use Une b/c NaN != x is true *)
      Llvm.build_fcmp Llvm.Fcmp.Une lv rv "neqtmp" builder
  | Less -> Llvm.build_fcmp Llvm.Fcmp.Olt lv rv "lttmp" builder
  | Leq -> Llvm.build_fcmp Llvm.Fcmp.Ole lv rv "letmp" builder
  | Greater -> Llvm.build_fcmp Llvm.Fcmp.Ogt lv rv "gttmp" builder
  | Geq -> Llvm.build_fcmp Llvm.Fcmp.Oge lv rv "getmp" builder
  | _ -> assert false [@coverage off]

and codegen_int_and_ptr_binop operand_typ op lhs rhs =
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
      if is_float_type (expr_type e) then Llvm.build_fneg v "fnegtmp" builder
      else Llvm.build_neg v "" builder
  | Not ->
      let v = codegen_expr e in
      let zero = Llvm.const_null (Llvm.type_of v) in
      if is_float_type (expr_type e) then
        Llvm.build_fcmp Llvm.Fcmp.Oeq v zero "nottmp" builder
      else Llvm.build_icmp Llvm.Icmp.Eq v zero "nottmp" builder
  | Compl ->
      (* only for ints *)
      let v = codegen_expr e in
      Llvm.build_not v "compltmp" builder
  | AddrOf -> begin
      (* lvalues only *)
      match e with
      | VarRef (Checked _, name) -> Hashtbl.find locals name
      | UnaryOp (Checked _, Deref, ptr_e) -> codegen_expr ptr_e
      | Subscript _ -> lvalue_ptr e
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
  let fn = ensure_function_declared name in
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
  | IntLiteral (Checked (_, t), n, _) -> Llvm.const_int (llvm_of_typ t) n
  | FloatLiteral (Checked _, f) -> Llvm.const_float float_type f
  | DoubleLiteral (Checked _, f) -> Llvm.const_float double_type f
  | LongDoubleLiteral (Checked _, f) -> Llvm.const_float double_type f
  | BoolLiteral (Checked _, b) ->
      (* bool literals produce i1 for computation *)
      Llvm.const_int bool_type (if b then 1 else 0)
  | CharLiteral (Checked _, c) -> Llvm.const_int char_type c
  | StringLiteral (_, s) -> codegen_string s
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
  | Cast (Checked _, t, e) -> codegen_cast (typ_of_source_type t) e
  | ImplicitCast (Checked _, t, e) -> codegen_cast t e
  | SizeofExpr (Checked _, e) ->
      Llvm.const_int long_type (sizeof_typ (expr_type e))
  | SizeofType (Checked _, t) ->
      Llvm.const_int long_type (sizeof_typ (typ_of_source_type t))
  | MemberAccess (Checked (_, t), lhs, field) -> begin
      let tag =
        match expr_type lhs with
        | Struct tag -> tag
        | _ -> assert false [@coverage off]
      in
      let struct_ptr = lvalue_ptr lhs in
      let field_ptr = member_field_ptr struct_ptr tag field in
      (* array fields decay to a pointer to the first element *)
      match t with
      | Array (elem_t, _) ->
          let zero = Llvm.const_int int_type 0 in
          Llvm.build_gep (llvm_of_typ elem_t) field_ptr [| zero |] "arrdecay"
            builder
      | _ -> emit_load t field_ptr "field"
    end
  | _ -> assert false [@coverage off]

(* find index and type of a named field in a field list *)
and find_field fields field_name =
  let rec go idx = function
    | [] -> None
    | (fname, ft) :: _ when fname = field_name -> Some (idx, ft)
    | _ :: rest -> go (idx + 1) rest
  in
  go 0 fields

(** gets the LLVM struct type and field index for a given tag and field name *)
and struct_field_info tag field_name =
  match Hashtbl.find_opt struct_defs tag with
  | None -> failwith ("struct not defined: " ^ tag)
  | Some (llty, fields) -> begin
      match find_field fields field_name with
      | None -> failwith ("no field '" ^ field_name ^ "' in struct " ^ tag)
      | Some (idx, ft) -> (llty, idx, ft)
    end

(** gets the pointer to a struct field via member access *)
and member_field_ptr struct_ptr tag field_name =
  let llty, idx, _ = struct_field_info tag field_name in
  let zero = Llvm.const_int int_type 0 in
  let fidx = Llvm.const_int int_type idx in
  Llvm.build_gep llty struct_ptr [| zero; fidx |] "fieldptr" builder

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
      | StringLiteral _ -> codegen_expr e
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
  | MemberAccess (Checked (_, _), lhs, field) -> begin
      let tag =
        match expr_type lhs with
        | Struct tag -> tag
        | _ -> assert false [@coverage off]
      in
      let struct_ptr = lvalue_ptr lhs in
      member_field_ptr struct_ptr tag field
    end
  | _ -> assert false [@coverage off]

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
    (* floats *)
    | `Inc, t when is_float_type t ->
        let one = Llvm.const_float ll_ty 1. in
        Llvm.build_fadd old_val one "incdec" builder
    | `Dec, t when is_float_type t ->
        let one = Llvm.const_float ll_ty 1. in
        Llvm.build_fsub old_val one "incdec" builder
    (* integers *)
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

and codegen_pointer_cast v from_t to_t =
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
  | _ -> assert false [@coverage off]

and codegen_bool_cast v from_t =
  (* we have to explicitly handle bool bc trunc may not necessarily work. ie,
     trunc i32 2 to i1 gives 0 not 1. it should be that any nonzero value is
     true. instead, we do a != 0 comparison *)
  let zero = Llvm.const_null (llvm_of_typ from_t) in
  if is_float_type from_t then
    Llvm.build_fcmp Llvm.Fcmp.One v zero "booltmp" builder
  else Llvm.build_icmp Llvm.Icmp.Ne v zero "booltmp" builder

and codegen_arithmetic_cast v from_t to_t =
  let to_ll = llvm_of_typ to_t in
  match (from_t, to_t) with
  | _, _ when is_float_type from_t && is_float_type to_t ->
      (* for floats, no concept of signedness *)
      let from_size = sizeof_typ from_t in
      let to_size = sizeof_typ to_t in
      if to_size < from_size then
        (* if we need to make the value smaller, we trunc *)
        Llvm.build_fptrunc v to_ll "fptrunctmp" builder (* ext *)
      else if to_size > from_size then
        (* if we need to make the value larger, we ext *)
        Llvm.build_fpext v to_ll "fpexttmp" builder
      else
        (* same size, don't do anything *)
        v
  | _, _ when is_integer_type from_t && is_float_type to_t ->
      if is_signed from_t then Llvm.build_sitofp v to_ll "sitofptmp" builder
      else Llvm.build_uitofp v to_ll "uitofptmp" builder
  | _, _ when is_float_type from_t && is_integer_type to_t ->
      if is_signed to_t then Llvm.build_fptosi v to_ll "fptositmp" builder
      else Llvm.build_fptoui v to_ll "fptouitmp" builder
  | _ ->
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

and codegen_cast to_t e =
  let v = codegen_expr e in
  let from_t = expr_type e in
  if from_t = to_t then
    (* already the same type, we don't need to do anything *)
    v
  else
    match (from_t, to_t) with
    | Array (elem_t, _), Ptr elem_t' when elem_t = elem_t' ->
        (* arrays already decay to pointers in expression codegen *)
        v
    | _ ->
        if to_t = Bool then
          (* convert everything to bool *)
          codegen_bool_cast v from_t
        else if
          (is_pointer_type from_t && is_pointer_type to_t)
          || (is_integer_type from_t && is_pointer_type to_t)
          || (is_pointer_type from_t && is_integer_type to_t)
        then
          (* for pointer related casts *)
          codegen_pointer_cast v from_t to_t
        else codegen_arithmetic_cast v from_t to_t

and codegen_string s =
  (* get a list of the bytes, and append the null terminator *)
  let bytes = Array.of_list (List.map (Llvm.const_int char_type) (s @ [ 0 ])) in
  let ty = Llvm.array_type char_type (Array.length bytes) in
  let name =
    let n = !string_literal_counter in
    incr string_literal_counter;
    Printf.sprintf ".str.%d" n
  in
  (* c string literals have static storage duration so we have to turn them into
     private global constants, rather than alloca *)
  let global =
    Llvm.define_global name (Llvm.const_array char_type bytes) the_module
  in
  Llvm.set_linkage Llvm.Linkage.Private global;
  Llvm.set_global_constant true global;
  Llvm.set_unnamed_addr true global;
  let zero = Llvm.const_int int_type 0 in
  (* the value of the expression is just a pointer to the first character *)
  Llvm.build_gep ty global [| zero; zero |] "str" builder
