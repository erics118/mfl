  $ dune exec mfl -- ir fixtures/incdec.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 0, ptr %x, align 4
    %incdec = load i32, ptr %x, align 4
    %incdec1 = add i32 %incdec, 1
    store i32 %incdec1, ptr %x, align 4
    %incdec2 = load i32, ptr %x, align 4
    %incdec3 = add i32 %incdec2, 1
    store i32 %incdec3, ptr %x, align 4
    %incdec4 = load i32, ptr %x, align 4
    %incdec5 = sub i32 %incdec4, 1
    store i32 %incdec5, ptr %x, align 4
    %incdec6 = load i32, ptr %x, align 4
    %incdec7 = sub i32 %incdec6, 1
    store i32 %incdec7, ptr %x, align 4
    %a = alloca i32, align 4
    %incdec8 = load i32, ptr %x, align 4
    %incdec9 = add i32 %incdec8, 1
    store i32 %incdec9, ptr %x, align 4
    store i32 %incdec9, ptr %a, align 4
    %b = alloca i32, align 4
    %incdec10 = load i32, ptr %x, align 4
    %incdec11 = add i32 %incdec10, 1
    store i32 %incdec11, ptr %x, align 4
    store i32 %incdec10, ptr %b, align 4
    %x12 = load i32, ptr %x, align 4
    ret i32 %x12
  }
