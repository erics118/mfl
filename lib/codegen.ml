let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context
let int_type = Llvm.i64_type context

(* optionally disable ssa so we can see exact code in the ll *)
let rec integer_to_llvm n =
  if false then Llvm.const_int int_type n
  else
    let ptr = Llvm.build_alloca int_type "numptr" builder in
    ignore (Llvm.build_store (Llvm.const_int int_type n) ptr builder);
    Llvm.build_load int_type ptr "num" builder

and binary_to_llvm op lhs rhs =
  let lv = expr_to_llvm lhs in
  let rv = expr_to_llvm rhs in
  match op with
  | Ast.Add -> Llvm.build_add lv rv "addtmp" builder
  | Ast.Sub -> Llvm.build_sub lv rv "subtmp" builder
  | Ast.Mul -> Llvm.build_mul lv rv "multmp" builder
  | Ast.Div -> Llvm.build_sdiv lv rv "divtmp" builder
  | _ -> failwith "unimplemented"

and expr_to_llvm = function
  | Ast.IntLiteral n -> integer_to_llvm n
  | Ast.BinaryOp (op, lhs, rhs) -> binary_to_llvm op lhs rhs
  | _ -> failwith "unimplemented"
