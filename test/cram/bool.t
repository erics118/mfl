  $ dune exec mfl -- ir fixtures/bool.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @bool_ops(i1 %x, i1 %y) {
  entry:
    %x1 = alloca i1, align 1
    store i1 %x, ptr %x1, align 1
    %y2 = alloca i1, align 1
    store i1 %y, ptr %y2, align 1
    %a = alloca i1, align 1
    %x3 = load i1, ptr %x1, align 1
    br i1 %x3, label %and_rhs, label %and_merge
  
  and_rhs:                                          ; preds = %entry
    %y4 = load i1, ptr %y2, align 1
    br label %and_merge
  
  and_merge:                                        ; preds = %and_rhs, %entry
    %andtmp = phi i1 [ %x3, %entry ], [ %y4, %and_rhs ]
    store i1 %andtmp, ptr %a, align 1
    %b = alloca i1, align 1
    %x5 = load i1, ptr %x1, align 1
    br i1 %x5, label %or_merge, label %or_rhs
  
  or_rhs:                                           ; preds = %and_merge
    %y6 = load i1, ptr %y2, align 1
    br label %or_merge
  
  or_merge:                                         ; preds = %or_rhs, %and_merge
    %ortmp = phi i1 [ %x5, %and_merge ], [ %y6, %or_rhs ]
    store i1 %ortmp, ptr %b, align 1
    %c = alloca i1, align 1
    store i1 true, ptr %c, align 1
    %d = alloca i1, align 1
    store i1 false, ptr %d, align 1
    ret i32 0
  }
