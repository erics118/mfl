  $ dune exec mfl -- ir fixtures/bool.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @bool_ops(i8 %x, i8 %y) {
  entry:
    %x1 = alloca i8, align 1
    store i8 %x, ptr %x1, align 1
    %y2 = alloca i8, align 1
    store i8 %y, ptr %y2, align 1
    %a = alloca i8, align 1
    %x3 = load i8, ptr %x1, align 1
    %x_b = trunc i8 %x3 to i1
    br i1 %x_b, label %and_rhs, label %and_merge
  
  and_rhs:                                          ; preds = %entry
    %y4 = load i8, ptr %y2, align 1
    %y_b = trunc i8 %y4 to i1
    br label %and_merge
  
  and_merge:                                        ; preds = %and_rhs, %entry
    %andtmp = phi i1 [ %x_b, %entry ], [ %y_b, %and_rhs ]
    %storeb = zext i1 %andtmp to i8
    store i8 %storeb, ptr %a, align 1
    %b = alloca i8, align 1
    %x5 = load i8, ptr %x1, align 1
    %x_b6 = trunc i8 %x5 to i1
    br i1 %x_b6, label %or_merge, label %or_rhs
  
  or_rhs:                                           ; preds = %and_merge
    %y7 = load i8, ptr %y2, align 1
    %y_b8 = trunc i8 %y7 to i1
    br label %or_merge
  
  or_merge:                                         ; preds = %or_rhs, %and_merge
    %ortmp = phi i1 [ %x_b6, %and_merge ], [ %y_b8, %or_rhs ]
    %storeb9 = zext i1 %ortmp to i8
    store i8 %storeb9, ptr %b, align 1
    %c = alloca i8, align 1
    store i8 1, ptr %c, align 1
    %d = alloca i8, align 1
    store i8 0, ptr %d, align 1
    ret i32 0
  }
