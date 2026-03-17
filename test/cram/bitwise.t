  $ dune exec mfl -- fixtures/bitwise.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @bitwise(i32 %x, i32 %y) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %y2 = alloca i32, align 4
    store i32 %y, ptr %y2, align 4
    %a = alloca i32, align 4
    %x3 = load i32, ptr %x1, align 4
    %y4 = load i32, ptr %y2, align 4
    %bandtmp = and i32 %x3, %y4
    store i32 %bandtmp, ptr %a, align 4
    %b = alloca i32, align 4
    %x5 = load i32, ptr %x1, align 4
    %y6 = load i32, ptr %y2, align 4
    %bortmp = or i32 %x5, %y6
    store i32 %bortmp, ptr %b, align 4
    %c = alloca i32, align 4
    %x7 = load i32, ptr %x1, align 4
    %y8 = load i32, ptr %y2, align 4
    %xortmp = xor i32 %x7, %y8
    store i32 %xortmp, ptr %c, align 4
    %c9 = load i32, ptr %c, align 4
    ret i32 %c9
  }
