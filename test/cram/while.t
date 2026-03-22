  $ dune exec mfl -- ir fixtures/while.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
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
    call void @printint(i32 %i3)
    br label %while_cond
  
  while_after:                                      ; preds = %while_cond
    ret i32 0
  }
