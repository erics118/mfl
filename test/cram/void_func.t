  $ dune exec mfl -- ir fixtures/void_func.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  @.str.0 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  
  declare i32 @printf(ptr, ...)
  
  define void @greet(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    %calltmp = call i32 (ptr, ...) @printf(ptr @.str.0, i32 %x2)
    ret void
  }
  
  define i32 @main() {
  entry:
    call void @greet(i32 42)
    ret i32 0
  }
