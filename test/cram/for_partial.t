  $ mfl ir fixtures/for_partial.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  define i32 @main() {
  entry:
    %i = alloca i32, align 4
    br label %for_init
  
  for_init:                                         ; preds = %entry
    br label %for_cond
  
  for_cond:                                         ; preds = %for_incr, %for_init
    br label %for_body
  
  for_incr:                                         ; No predecessors!
    br label %for_cond
  
  for_body:                                         ; preds = %for_cond
    ret i32 0
  
  for_after:                                        ; No predecessors!
    br label %for_init1
  
  for_init1:                                        ; preds = %for_after
    store i32 0, ptr %i, align 4
    br label %for_cond2
  
  for_cond2:                                        ; preds = %for_incr3, %for_init1
    br label %for_body4
  
  for_incr3:                                        ; No predecessors!
    br label %for_cond2
  
  for_body4:                                        ; preds = %for_cond2
    %i6 = load i32, ptr %i, align 4
    ret i32 %i6
  
  for_after5:                                       ; No predecessors!
    br label %for_init7
  
  for_init7:                                        ; preds = %for_after5
    br label %for_cond8
  
  for_cond8:                                        ; preds = %for_incr9, %for_init7
    %i12 = load i32, ptr %i, align 4
    %lttmp = icmp slt i32 %i12, 10
    br i1 %lttmp, label %for_body10, label %for_after11
  
  for_incr9:                                        ; preds = %for_body10
    br label %for_cond8
  
  for_body10:                                       ; preds = %for_cond8
    %incdec = load i32, ptr %i, align 4
    %incdec13 = add i32 %incdec, 1
    store i32 %incdec13, ptr %i, align 4
    br label %for_incr9
  
  for_after11:                                      ; preds = %for_cond8
    br label %for_init14
  
  for_init14:                                       ; preds = %for_after11
    br label %for_cond15
  
  for_cond15:                                       ; preds = %for_incr16, %for_init14
    br label %for_body17
  
  for_incr16:                                       ; No predecessors!
    %incdec20 = load i32, ptr %i, align 4
    %incdec21 = add i32 %incdec20, 1
    store i32 %incdec21, ptr %i, align 4
    br label %for_cond15
  
  for_body17:                                       ; preds = %for_cond15
    %i19 = load i32, ptr %i, align 4
    ret i32 %i19
  
  for_after18:                                      ; No predecessors!
    br label %for_init22
  
  for_init22:                                       ; preds = %for_after18
    %j = alloca i32, align 4
    store i32 0, ptr %j, align 4
    br label %for_cond23
  
  for_cond23:                                       ; preds = %for_incr24, %for_init22
    %j27 = load i32, ptr %j, align 4
    %lttmp28 = icmp slt i32 %j27, 10
    br i1 %lttmp28, label %for_body25, label %for_after26
  
  for_incr24:                                       ; preds = %for_body25
    br label %for_cond23
  
  for_body25:                                       ; preds = %for_cond23
    %incdec29 = load i32, ptr %j, align 4
    %incdec30 = add i32 %incdec29, 1
    store i32 %incdec30, ptr %j, align 4
    br label %for_incr24
  
  for_after26:                                      ; preds = %for_cond23
    br label %for_init31
  
  for_init31:                                       ; preds = %for_after26
    %j36 = alloca i32, align 4
    store i32 0, ptr %j36, align 4
    br label %for_cond32
  
  for_cond32:                                       ; preds = %for_incr33, %for_init31
    br label %for_body34
  
  for_incr33:                                       ; No predecessors!
    %incdec38 = load i32, ptr %j36, align 4
    %incdec39 = add i32 %incdec38, 1
    store i32 %incdec39, ptr %j36, align 4
    br label %for_cond32
  
  for_body34:                                       ; preds = %for_cond32
    %j37 = load i32, ptr %j36, align 4
    ret i32 %j37
  
  for_after35:                                      ; No predecessors!
    br label %for_init40
  
  for_init40:                                       ; preds = %for_after35
    br label %for_cond41
  
  for_cond41:                                       ; preds = %for_incr42, %for_init40
    %i45 = load i32, ptr %i, align 4
    %lttmp46 = icmp slt i32 %i45, 10
    br i1 %lttmp46, label %for_body43, label %for_after44
  
  for_incr42:                                       ; preds = %for_body43
    %incdec47 = load i32, ptr %i, align 4
    %incdec48 = add i32 %incdec47, 1
    store i32 %incdec48, ptr %i, align 4
    br label %for_cond41
  
  for_body43:                                       ; preds = %for_cond41
    br label %for_incr42
  
  for_after44:                                      ; preds = %for_cond41
    %i49 = load i32, ptr %i, align 4
    ret i32 %i49
  }
