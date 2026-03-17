  $ dune exec mfl -- fixtures/cmp.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @cmp(i32 %x, i32 %y) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %y2 = alloca i32, align 4
    store i32 %y, ptr %y2, align 4
    %a = alloca i1, align 1
    %x3 = load i32, ptr %x1, align 4
    %y4 = load i32, ptr %y2, align 4
    %eqtmp = icmp eq i32 %x3, %y4
    store i1 %eqtmp, ptr %a, align 1
    %b = alloca i1, align 1
    %x5 = load i32, ptr %x1, align 4
    %y6 = load i32, ptr %y2, align 4
    %neqtmp = icmp ne i32 %x5, %y6
    store i1 %neqtmp, ptr %b, align 1
    %c = alloca i1, align 1
    %x7 = load i32, ptr %x1, align 4
    %y8 = load i32, ptr %y2, align 4
    %lttmp = icmp slt i32 %x7, %y8
    store i1 %lttmp, ptr %c, align 1
    %d = alloca i1, align 1
    %x9 = load i32, ptr %x1, align 4
    %y10 = load i32, ptr %y2, align 4
    %letmp = icmp sle i32 %x9, %y10
    store i1 %letmp, ptr %d, align 1
    %e = alloca i1, align 1
    %x11 = load i32, ptr %x1, align 4
    %y12 = load i32, ptr %y2, align 4
    %gttmp = icmp sgt i32 %x11, %y12
    store i1 %gttmp, ptr %e, align 1
    %r = alloca i1, align 1
    %x13 = load i32, ptr %x1, align 4
    %y14 = load i32, ptr %y2, align 4
    %getmp = icmp sge i32 %x13, %y14
    store i1 %getmp, ptr %r, align 1
    ret i32 0
  }
