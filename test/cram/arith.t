  $ dune exec mfl -- fixtures/arith.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @arith(i32 %x, i32 %y) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %y2 = alloca i32, align 4
    store i32 %y, ptr %y2, align 4
    %a = alloca i32, align 4
    %x3 = load i32, ptr %x1, align 4
    %y4 = load i32, ptr %y2, align 4
    %addtmp = add i32 %x3, %y4
    store i32 %addtmp, ptr %a, align 4
    %b = alloca i32, align 4
    %x5 = load i32, ptr %x1, align 4
    %y6 = load i32, ptr %y2, align 4
    %subtmp = sub i32 %x5, %y6
    store i32 %subtmp, ptr %b, align 4
    %c = alloca i32, align 4
    %x7 = load i32, ptr %x1, align 4
    %y8 = load i32, ptr %y2, align 4
    %multmp = mul i32 %x7, %y8
    store i32 %multmp, ptr %c, align 4
    %d = alloca i32, align 4
    %x9 = load i32, ptr %x1, align 4
    %y10 = load i32, ptr %y2, align 4
    %divtmp = sdiv i32 %x9, %y10
    store i32 %divtmp, ptr %d, align 4
    %e = alloca i32, align 4
    %x11 = load i32, ptr %x1, align 4
    %y12 = load i32, ptr %y2, align 4
    %modtmp = srem i32 %x11, %y12
    store i32 %modtmp, ptr %e, align 4
    %e13 = load i32, ptr %e, align 4
    ret i32 %e13
  }
