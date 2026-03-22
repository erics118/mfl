  $ dune exec mfl -- ir fixtures/cmp.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @cmp(i32 %x, i32 %y) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %y2 = alloca i32, align 4
    store i32 %y, ptr %y2, align 4
    %a = alloca i8, align 1
    %x3 = load i32, ptr %x1, align 4
    %y4 = load i32, ptr %y2, align 4
    %eqtmp = icmp eq i32 %x3, %y4
    %storeb = zext i1 %eqtmp to i8
    store i8 %storeb, ptr %a, align 1
    %b = alloca i8, align 1
    %x5 = load i32, ptr %x1, align 4
    %y6 = load i32, ptr %y2, align 4
    %neqtmp = icmp ne i32 %x5, %y6
    %storeb7 = zext i1 %neqtmp to i8
    store i8 %storeb7, ptr %b, align 1
    %c = alloca i8, align 1
    %x8 = load i32, ptr %x1, align 4
    %y9 = load i32, ptr %y2, align 4
    %lttmp = icmp slt i32 %x8, %y9
    %storeb10 = zext i1 %lttmp to i8
    store i8 %storeb10, ptr %c, align 1
    %d = alloca i8, align 1
    %x11 = load i32, ptr %x1, align 4
    %y12 = load i32, ptr %y2, align 4
    %letmp = icmp sle i32 %x11, %y12
    %storeb13 = zext i1 %letmp to i8
    store i8 %storeb13, ptr %d, align 1
    %e = alloca i8, align 1
    %x14 = load i32, ptr %x1, align 4
    %y15 = load i32, ptr %y2, align 4
    %gttmp = icmp sgt i32 %x14, %y15
    %storeb16 = zext i1 %gttmp to i8
    store i8 %storeb16, ptr %e, align 1
    %r = alloca i8, align 1
    %x17 = load i32, ptr %x1, align 4
    %y18 = load i32, ptr %y2, align 4
    %getmp = icmp sge i32 %x17, %y18
    %storeb19 = zext i1 %getmp to i8
    store i8 %storeb19, ptr %r, align 1
    ret i32 0
  }
