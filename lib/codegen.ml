let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context
let int_type = Llvm.i32_type context
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
  List.iteri
    (fun i (_, pname) -> Llvm.set_value_name pname (Llvm.param fn i))
    params;
  Llvm.position_at_end (Llvm.entry_block fn) builder;
  List.iter codegen_stmt body

and codegen_stmt = function
  | Ast.FuncDef { ret_type; name; params; body } ->
      codegen_func ret_type name params body
  | Ast.ReturnStmt (Some e) -> ignore (Llvm.build_ret (codegen_expr e) builder)
  | Ast.ReturnStmt None -> ignore (Llvm.build_ret_void builder)
  | _ -> failwith "unimplemented"

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
