  $ dune exec mfl -- ir fixtures/ternary.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @ternary(i8 %cond, i32 %x, i32 %y) {
  entry:
    %cond1 = alloca i8, align 1
    store i8 %cond, ptr %cond1, align 1
    %x2 = alloca i32, align 4
    store i32 %x, ptr %x2, align 4
    %y3 = alloca i32, align 4
    store i32 %y, ptr %y3, align 4
    %cond4 = load i8, ptr %cond1, align 1
    %cond_b = trunc i8 %cond4 to i1
    br i1 %cond_b, label %ternary_then, label %ternary_else
  
  ternary_then:                                     ; preds = %entry
    %x5 = load i32, ptr %x2, align 4
    br label %ternary_merge
  
  ternary_else:                                     ; preds = %entry
    %y6 = load i32, ptr %y3, align 4
    br label %ternary_merge
  
  ternary_merge:                                    ; preds = %ternary_else, %ternary_then
    %ternary = phi i32 [ %x5, %ternary_then ], [ %y6, %ternary_else ]
    ret i32 %ternary
  }
