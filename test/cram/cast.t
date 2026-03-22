  $ dune exec mfl -- ir fixtures/cast.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @trunc_cast(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 4
    %x2 = load i64, ptr %x1, align 4
    %trunctmp = trunc i64 %x2 to i32
    ret i32 %trunctmp
  }
  
  define i64 @sext_cast(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    %sexttmp = sext i32 %x2 to i64
    ret i64 %sexttmp
  }
  
  define i8 @to_bool(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    %booltmp = icmp ne i32 %x2, 0
    %retb = zext i1 %booltmp to i8
    ret i8 %retb
  }
  
  define i32 @widen_char(i8 %c) {
  entry:
    %c1 = alloca i8, align 1
    store i8 %c, ptr %c1, align 1
    %c2 = load i8, ptr %c1, align 1
    %sexttmp = sext i8 %c2 to i32
    ret i32 %sexttmp
  }
