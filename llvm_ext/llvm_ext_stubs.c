#include <llvm-c/Core.h>
#include <caml/mlvalues.h>

// to_val/from_val encode LLVM C API pointers as OCaml integers
extern value to_val(void *ptr);
extern void *from_val(value v);

// LLVMGlobalGetValueType returns the underlying type of a GlobalValue
// (functions, global variables, aliases). Unlike LLVMTypeOf, this returns the
// actual type rather than a pointer type in opaque-pointer mode.
CAMLprim value llvm_global_value_type(value global) {
  return to_val(LLVMGlobalGetValueType((LLVMValueRef)from_val(global)));
}
