  $ dune exec mfl -- ir fixtures/void_func.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define void @greet(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    call void @printint(i32 %x2)
    ret void
  }
  
  define i32 @main() {
  entry:
    call void @greet(i32 42)
    ret i32 0
  }
