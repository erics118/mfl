  $ mfl ir fixtures/if_only.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  @.str.0 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
  
  declare i32 @printf(ptr, ...)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 5, ptr %x, align 4
    %x1 = load i32, ptr %x, align 4
    %gttmp = icmp sgt i32 %x1, 3
    br i1 %gttmp, label %then, label %else
  
  then:                                             ; preds = %entry
    %x2 = load i32, ptr %x, align 4
    %calltmp = call i32 (ptr, ...) @printf(ptr @.str.0, i32 %x2)
    br label %merge
  
  else:                                             ; preds = %entry
    br label %merge
  
  merge:                                            ; preds = %else, %then
    ret i32 0
  }
