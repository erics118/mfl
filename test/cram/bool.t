  $ mfl ir fixtures/bool.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  define i32 @bool_ops(i1 zeroext %x, i1 zeroext %y) {
  entry:
    %x1 = alloca i8, align 1
    %storeb = zext i1 %x to i8
    store i8 %storeb, ptr %x1, align 1
    %y2 = alloca i8, align 1
    %storeb3 = zext i1 %y to i8
    store i8 %storeb3, ptr %y2, align 1
    %a = alloca i8, align 1
    %x4 = load i8, ptr %x1, align 1
    %x_b = trunc i8 %x4 to i1
    br i1 %x_b, label %and_rhs, label %and_merge
  
  and_rhs:                                          ; preds = %entry
    %y5 = load i8, ptr %y2, align 1
    %y_b = trunc i8 %y5 to i1
    br label %and_merge
  
  and_merge:                                        ; preds = %and_rhs, %entry
    %andtmp = phi i1 [ %x_b, %entry ], [ %y_b, %and_rhs ]
    %storeb6 = zext i1 %andtmp to i8
    store i8 %storeb6, ptr %a, align 1
    %b = alloca i8, align 1
    %x7 = load i8, ptr %x1, align 1
    %x_b8 = trunc i8 %x7 to i1
    br i1 %x_b8, label %or_merge, label %or_rhs
  
  or_rhs:                                           ; preds = %and_merge
    %y9 = load i8, ptr %y2, align 1
    %y_b10 = trunc i8 %y9 to i1
    br label %or_merge
  
  or_merge:                                         ; preds = %or_rhs, %and_merge
    %ortmp = phi i1 [ %x_b8, %and_merge ], [ %y_b10, %or_rhs ]
    %storeb11 = zext i1 %ortmp to i8
    store i8 %storeb11, ptr %b, align 1
    %c = alloca i8, align 1
    store i8 1, ptr %c, align 1
    %d = alloca i8, align 1
    store i8 0, ptr %d, align 1
    ret i32 0
  }
