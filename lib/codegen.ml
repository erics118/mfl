let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mfl"
let builder = Llvm.builder context
let double_type = Llvm.double_type context

let rec expr_to_llvm = function
  | Ast.Number n -> Llvm.const_float double_type n
  | Ast.Binary (op, lhs, rhs) -> (
      let lv = expr_to_llvm lhs in
      let rv = expr_to_llvm rhs in
      match op with
      | Ast.Add -> Llvm.build_fadd lv rv "addtmp" builder
      | Ast.Sub -> Llvm.build_fsub lv rv "subtmp" builder
      | Ast.Mul -> Llvm.build_fmul lv rv "multmp" builder
      | Ast.Div -> Llvm.build_fdiv lv rv "divtmp" builder)
