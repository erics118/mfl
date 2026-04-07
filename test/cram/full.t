  $ mfl ir fixtures/full.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  @.str.0 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  @.str.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  @.str.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  
  declare i32 @printf(ptr, ...)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 5, ptr %x, align 4
    %x1 = load i32, ptr %x, align 4
    %eqtmp = icmp eq i32 %x1, 3
    %zexttmp = zext i1 %eqtmp to i32
    %calltmp = call i32 (ptr, ...) @printf(ptr @.str.0, i32 %zexttmp)
    %x2 = load i32, ptr %x, align 4
    %eqtmp3 = icmp eq i32 %x2, 3
    br i1 %eqtmp3, label %then, label %else
  
  then:                                             ; preds = %entry
    %x4 = load i32, ptr %x, align 4
    %calltmp5 = call i32 (ptr, ...) @printf(ptr @.str.1, i32 %x4)
    br label %merge
  
  else:                                             ; preds = %entry
    %x6 = load i32, ptr %x, align 4
    %subtmp = sub i32 %x6, 1
    %calltmp7 = call i32 (ptr, ...) @printf(ptr @.str.2, i32 %subtmp)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    ret i32 0
  }
