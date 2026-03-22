  $ dune exec mfl -- ir fixtures/int_types_arith.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    %c = alloca i8, align 1
    store i8 100, ptr %c, align 1
    %c1 = load i8, ptr %c, align 1
    %sexttmp = sext i8 %c1 to i32
    %addtmp = add i32 %sexttmp, 5
    %trunctmp = trunc i32 %addtmp to i8
    store i8 %trunctmp, ptr %c, align 1
    %c2 = load i8, ptr %c, align 1
    %sexttmp3 = sext i8 %c2 to i32
    %subtmp = sub i32 %sexttmp3, 3
    %trunctmp4 = trunc i32 %subtmp to i8
    store i8 %trunctmp4, ptr %c, align 1
    %u = alloca i32, align 4
    store i32 100, ptr %u, align 4
    %u5 = load i32, ptr %u, align 4
    %addtmp6 = add i32 %u5, 5
    store i32 %addtmp6, ptr %u, align 4
    %u7 = load i32, ptr %u, align 4
    %divtmp = udiv i32 %u7, 3
    store i32 %divtmp, ptr %u, align 4
    %u8 = load i32, ptr %u, align 4
    %modtmp = urem i32 %u8, 7
    store i32 %modtmp, ptr %u, align 4
    %l = alloca i64, align 8
    store i64 1000000, ptr %l, align 4
    %l9 = load i64, ptr %l, align 4
    %multmp = mul i64 %l9, 2
    store i64 %multmp, ptr %l, align 4
    %ll = alloca i64, align 8
    store i64 1000000, ptr %ll, align 4
    %ll10 = load i64, ptr %ll, align 4
    %multmp11 = mul i64 %ll10, 2
    store i64 %multmp11, ptr %ll, align 4
    ret i32 0
  }
