  $ dune exec mfl -- ir fixtures/print.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  @.str.0 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  
  declare i32 @printf(ptr, ...)
  
  define i32 @main() {
  entry:
    %calltmp = call i32 (ptr, ...) @printf(ptr @.str.0, i32 6)
    ret i32 0
  }
