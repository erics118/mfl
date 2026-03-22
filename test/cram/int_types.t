  $ dune exec mfl -- ir fixtures/int_types.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    %c = alloca i8, align 1
    store i8 10, ptr %c, align 1
    %s = alloca i16, align 2
    store i16 1000, ptr %s, align 2
    %i = alloca i32, align 4
    store i32 1000, ptr %i, align 4
    %l = alloca i64, align 8
    store i64 100000, ptr %l, align 4
    %ll = alloca i64, align 8
    store i64 100000, ptr %ll, align 4
    %si = alloca i16, align 2
    store i16 1000, ptr %si, align 2
    %li = alloca i64, align 8
    store i64 100000, ptr %li, align 4
    %lli = alloca i64, align 8
    store i64 100000, ptr %lli, align 4
    %sc = alloca i8, align 1
    store i8 10, ptr %sc, align 1
    %ss = alloca i16, align 2
    store i16 1000, ptr %ss, align 2
    %si1 = alloca i32, align 4
    store i32 1000, ptr %si1, align 4
    %sl = alloca i64, align 8
    store i64 100000, ptr %sl, align 4
    %sll = alloca i64, align 8
    store i64 100000, ptr %sll, align 4
    %ssi = alloca i16, align 2
    store i16 1000, ptr %ssi, align 2
    %sli = alloca i64, align 8
    store i64 100000, ptr %sli, align 4
    %slli = alloca i64, align 8
    store i64 100000, ptr %slli, align 4
    %uc = alloca i8, align 1
    store i8 10, ptr %uc, align 1
    %us = alloca i16, align 2
    store i16 1000, ptr %us, align 2
    %ui = alloca i32, align 4
    store i32 1000, ptr %ui, align 4
    %ul = alloca i64, align 8
    store i64 100000, ptr %ul, align 4
    %ull = alloca i64, align 8
    store i64 100000, ptr %ull, align 4
    %usi = alloca i16, align 2
    store i16 1000, ptr %usi, align 2
    %uli = alloca i64, align 8
    store i64 100000, ptr %uli, align 4
    %ulli = alloca i64, align 8
    store i64 100000, ptr %ulli, align 4
    ret i32 0
  }
