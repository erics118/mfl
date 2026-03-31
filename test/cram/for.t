  $ dune exec mfl -- ir fixtures/for.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  define i32 @main() {
  entry:
    br label %for_init
  
  for_init:                                         ; preds = %entry
    %i = alloca i32, align 4
    store i32 0, ptr %i, align 4
    br label %for_cond
  
  for_cond:                                         ; preds = %for_incr, %for_init
    %i1 = load i32, ptr %i, align 4
    %lttmp = icmp slt i32 %i1, 10
    br i1 %lttmp, label %for_body, label %for_after
  
  for_incr:                                         ; preds = %for_body
    %i3 = load i32, ptr %i, align 4
    %addtmp = add i32 %i3, 1
    store i32 %addtmp, ptr %i, align 4
    br label %for_cond
  
  for_body:                                         ; preds = %for_cond
    %i2 = load i32, ptr %i, align 4
    call void @printint(i32 %i2)
    br label %for_incr
  
  for_after:                                        ; preds = %for_cond
    ret i32 0
  }
