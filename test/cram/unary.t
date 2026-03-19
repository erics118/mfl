  $ dune exec mfl -- ir fixtures/unary.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @unary(i32 %x, i1 %b) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %b2 = alloca i1, align 1
    store i1 %b, ptr %b2, align 1
    %a = alloca i32, align 4
    %x3 = load i32, ptr %x1, align 4
    %0 = sub i32 0, %x3
    store i32 %0, ptr %a, align 4
    %c = alloca i1, align 1
    %b4 = load i1, ptr %b2, align 1
    %nottmp = xor i1 %b4, true
    store i1 %nottmp, ptr %c, align 1
    %a5 = load i32, ptr %a, align 4
    ret i32 %a5
  }
