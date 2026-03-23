  $ dune exec mfl -- ir fixtures/empty_for.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    %i = alloca i32, align 4
    br label %for_init
  
  for_init:                                         ; preds = %entry
    store i32 0, ptr %i, align 4
    br label %for_cond
  
  for_cond:                                         ; preds = %for_incr, %for_init
    %i1 = load i32, ptr %i, align 4
    %lttmp = icmp slt i32 %i1, 3
    br i1 %lttmp, label %for_body, label %for_after
  
  for_incr:                                         ; preds = %for_body
    %incdec = load i32, ptr %i, align 4
    %incdec3 = add i32 %incdec, 1
    store i32 %incdec3, ptr %i, align 4
    br label %for_cond
  
  for_body:                                         ; preds = %for_cond
    %i2 = load i32, ptr %i, align 4
    call void @printint(i32 %i2)
    br label %for_incr
  
  for_after:                                        ; preds = %for_cond
    br label %for_init4
  
  for_init4:                                        ; preds = %for_after
    %j = alloca i32, align 4
    store i32 0, ptr %j, align 4
    br label %for_cond5
  
  for_cond5:                                        ; preds = %for_incr6, %for_init4
    br label %for_body7
  
  for_incr6:                                        ; preds = %merge
    %incdec11 = load i32, ptr %j, align 4
    %incdec12 = add i32 %incdec11, 1
    store i32 %incdec12, ptr %j, align 4
    br label %for_cond5
  
  for_body7:                                        ; preds = %for_cond5
    %j9 = load i32, ptr %j, align 4
    %getmp = icmp sge i32 %j9, 3
    br i1 %getmp, label %then, label %else
  
  for_after8:                                       ; No predecessors!
    ret i32 0
  
  then:                                             ; preds = %for_body7
    %i10 = load i32, ptr %i, align 4
    ret i32 %i10
  
  else:                                             ; preds = %for_body7
    br label %merge
  
  merge:                                            ; preds = %else
    br label %for_incr6
  }
