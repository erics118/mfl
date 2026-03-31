  $ dune exec mfl -- ir fixtures/break_continue.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  define i32 @main() {
  entry:
    %i = alloca i32, align 4
    store i32 0, ptr %i, align 4
    br label %while_cond
  
  while_cond:                                       ; preds = %merge, %entry
    br i1 true, label %while_body, label %while_after
  
  while_body:                                       ; preds = %while_cond
    %i1 = load i32, ptr %i, align 4
    %getmp = icmp sge i32 %i1, 5
    br i1 %getmp, label %then, label %else
  
  while_after:                                      ; preds = %then, %while_cond
    %j = alloca i32, align 4
    store i32 0, ptr %j, align 4
    %sum = alloca i32, align 4
    store i32 0, ptr %sum, align 4
    br label %while_cond3
  
  then:                                             ; preds = %while_body
    br label %while_after
  
  else:                                             ; preds = %while_body
    br label %merge
  
  merge:                                            ; preds = %else
    %i2 = load i32, ptr %i, align 4
    %addtmp = add i32 %i2, 1
    store i32 %addtmp, ptr %i, align 4
    br label %while_cond
  
  while_cond3:                                      ; preds = %merge12, %then10, %while_after
    %j6 = load i32, ptr %j, align 4
    %lttmp = icmp slt i32 %j6, 10
    br i1 %lttmp, label %while_body4, label %while_after5
  
  while_body4:                                      ; preds = %while_cond3
    %j7 = load i32, ptr %j, align 4
    %addtmp8 = add i32 %j7, 1
    store i32 %addtmp8, ptr %j, align 4
    %j9 = load i32, ptr %j, align 4
    %modtmp = srem i32 %j9, 2
    %eqtmp = icmp eq i32 %modtmp, 0
    br i1 %eqtmp, label %then10, label %else11
  
  while_after5:                                     ; preds = %while_cond3
    %i16 = load i32, ptr %i, align 4
    call void @printint(i32 %i16)
    %sum17 = load i32, ptr %sum, align 4
    call void @printint(i32 %sum17)
    ret i32 0
  
  then10:                                           ; preds = %while_body4
    br label %while_cond3
  
  else11:                                           ; preds = %while_body4
    br label %merge12
  
  merge12:                                          ; preds = %else11
    %sum13 = load i32, ptr %sum, align 4
    %j14 = load i32, ptr %j, align 4
    %addtmp15 = add i32 %sum13, %j14
    store i32 %addtmp15, ptr %sum, align 4
    br label %while_cond3
  }
