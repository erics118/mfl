  $ dune exec mfl -- fixtures/assign.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 1, ptr %x, align 4
    %z = alloca i32, align 4
    store i32 3, ptr %x, align 4
    store i32 3, ptr %z, align 4
    %z1 = load i32, ptr %z, align 4
    ret i32 %z1
  }
