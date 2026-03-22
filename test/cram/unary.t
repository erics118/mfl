  $ dune exec mfl -- ir fixtures/unary.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @unary(i32 %x, i1 zeroext %b) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %b2 = alloca i8, align 1
    %storeb = zext i1 %b to i8
    store i8 %storeb, ptr %b2, align 1
    %a = alloca i32, align 4
    %x3 = load i32, ptr %x1, align 4
    %0 = sub i32 0, %x3
    store i32 %0, ptr %a, align 4
    %c = alloca i8, align 1
    %b4 = load i8, ptr %b2, align 1
    %b_b = trunc i8 %b4 to i1
    %nottmp = icmp eq i1 %b_b, false
    %storeb5 = zext i1 %nottmp to i8
    store i8 %storeb5, ptr %c, align 1
    %a6 = load i32, ptr %a, align 4
    ret i32 %a6
  }
