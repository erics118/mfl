  $ dune exec mfl -- ir fixtures/uninit_var.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    %flag = alloca i1, align 1
    store i32 5, ptr %x, align 4
    store i1 true, ptr %flag, align 1
    %x1 = load i32, ptr %x, align 4
    ret i32 %x1
  }
