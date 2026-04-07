  $ mfl ir fixtures/while.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  @.str.0 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  
  declare i32 @printf(ptr, ...)
  
  define i32 @main() {
  entry:
    %i = alloca i32, align 4
    store i32 10, ptr %i, align 4
    br label %while_cond
  
  while_cond:                                       ; preds = %while_body, %entry
    %i1 = load i32, ptr %i, align 4
    %gttmp = icmp sgt i32 %i1, 0
    br i1 %gttmp, label %while_body, label %while_after
  
  while_body:                                       ; preds = %while_cond
    %i2 = load i32, ptr %i, align 4
    %subtmp = sub i32 %i2, 1
    store i32 %subtmp, ptr %i, align 4
    %i3 = load i32, ptr %i, align 4
    %calltmp = call i32 (ptr, ...) @printf(ptr @.str.0, i32 %i3)
    br label %while_cond
  
  while_after:                                      ; preds = %while_cond
    ret i32 0
  }
