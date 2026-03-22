  $ dune exec mfl -- ir fixtures/incdec.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 0, ptr %x, align 4
    %x1 = load i32, ptr %x, align 4
    %incdec = add i32 %x1, 1
    store i32 %incdec, ptr %x, align 4
    %x2 = load i32, ptr %x, align 4
    %incdec3 = add i32 %x2, 1
    store i32 %incdec3, ptr %x, align 4
    %x4 = load i32, ptr %x, align 4
    %incdec5 = sub i32 %x4, 1
    store i32 %incdec5, ptr %x, align 4
    %x6 = load i32, ptr %x, align 4
    %incdec7 = sub i32 %x6, 1
    store i32 %incdec7, ptr %x, align 4
    %a = alloca i32, align 4
    %x8 = load i32, ptr %x, align 4
    %incdec9 = add i32 %x8, 1
    store i32 %incdec9, ptr %x, align 4
    store i32 %incdec9, ptr %a, align 4
    %b = alloca i32, align 4
    %x10 = load i32, ptr %x, align 4
    %incdec11 = add i32 %x10, 1
    store i32 %incdec11, ptr %x, align 4
    store i32 %x10, ptr %b, align 4
    %x12 = load i32, ptr %x, align 4
    ret i32 %x12
  }
