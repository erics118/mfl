let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context
let int_type = Llvm.i64_type context
let bool_type = Llvm.i1_type context

(* config to enable/disable ssa, only so we can see the un-optimized values *)
let ssa = false

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

and codegen_expr = function
  | Ast.IntLiteral n -> codegen_int n
  | Ast.BoolLiteral b -> Llvm.const_int bool_type (if b then 1 else 0)
  | Ast.BinaryOp (op, lhs, rhs) -> codegen_binop op lhs rhs
  | _ -> failwith "unimplemented"

let codegen_program expr =
  let printint_ty =
    Llvm.function_type (Llvm.void_type context) [| int_type |]
  in
  let printint_fn = Llvm.declare_function "printint" printint_ty the_module in
  let printbool_ty =
    Llvm.function_type (Llvm.void_type context) [| bool_type |]
  in
  let printbool_fn =
    Llvm.declare_function "printbool" printbool_ty the_module
  in
  let main_ty = Llvm.function_type (Llvm.i32_type context) [||] in
  let main_fn = Llvm.define_function "main" main_ty the_module in
  Llvm.position_at_end (Llvm.entry_block main_fn) builder;
  let result = codegen_expr expr in
  let print_fn, print_ty =
    if Llvm.type_of result = int_type then (printint_fn, printint_ty)
    else (printbool_fn, printbool_ty)
  in
  ignore (Llvm.build_call print_ty print_fn [| result |] "" builder);
  ignore (Llvm.build_ret (Llvm.const_int (Llvm.i32_type context) 0) builder)

let emit_ir () = Llvm.string_of_llmodule the_module
