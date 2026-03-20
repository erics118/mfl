  $ dune exec mfl -- ir fixtures/int_types_arith.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @main() {
  entry:
    %c = alloca i8, align 1
    store i8 100, ptr %c, align 1
    %c1 = load i8, ptr %c, align 1
    %addtmp = add i8 %c1, 5
    store i8 %addtmp, ptr %c, align 1
    %c2 = load i8, ptr %c, align 1
    %subtmp = sub i8 %c2, 3
    store i8 %subtmp, ptr %c, align 1
    %u = alloca i32, align 4
    store i32 100, ptr %u, align 4
    %u3 = load i32, ptr %u, align 4
    %addtmp4 = add i32 %u3, 5
    store i32 %addtmp4, ptr %u, align 4
    %u5 = load i32, ptr %u, align 4
    %divtmp = udiv i32 %u5, 3
    store i32 %divtmp, ptr %u, align 4
    %u6 = load i32, ptr %u, align 4
    %modtmp = urem i32 %u6, 7
    store i32 %modtmp, ptr %u, align 4
    %l = alloca i64, align 8
    store i64 1000000, ptr %l, align 4
    %l7 = load i64, ptr %l, align 4
    %multmp = mul i64 %l7, 2
    store i64 %multmp, ptr %l, align 4
    %ll = alloca i64, align 8
    store i64 1000000, ptr %ll, align 4
    %ll8 = load i64, ptr %ll, align 4
    %multmp9 = mul i64 %ll8, 2
    store i64 %multmp9, ptr %ll, align 4
    ret i32 0
  }
