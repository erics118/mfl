
  $ dune exec mfl -- ir fixtures/break_for.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @main() {
  entry:
    %result = alloca i32, align 4
    store i32 0, ptr %result, align 4
    br label %for_init
  
  for_init:                                         ; preds = %entry
    %i = alloca i32, align 4
    store i32 0, ptr %i, align 4
    br label %for_cond
  
  for_cond:                                         ; preds = %for_incr, %for_init
    %i1 = load i32, ptr %i, align 4
    %lttmp = icmp slt i32 %i1, 100
    br i1 %lttmp, label %for_body, label %for_after
  
  for_incr:                                         ; preds = %merge
    %i5 = load i32, ptr %i, align 4
    %incdec = add i32 %i5, 1
    store i32 %incdec, ptr %i, align 4
    br label %for_cond
  
  for_body:                                         ; preds = %for_cond
    %i2 = load i32, ptr %i, align 4
    %eqtmp = icmp eq i32 %i2, 10
    br i1 %eqtmp, label %then, label %else
  
  for_after:                                        ; preds = %then, %for_cond
    %result6 = load i32, ptr %result, align 4
    ret i32 %result6
  
  then:                                             ; preds = %for_body
    br label %for_after
  
  else:                                             ; preds = %for_body
    br label %merge
  
  merge:                                            ; preds = %else
    %result3 = load i32, ptr %result, align 4
    %i4 = load i32, ptr %i, align 4
    %addtmp = add i32 %result3, %i4
    store i32 %addtmp, ptr %result, align 4
    br label %for_incr
  }
