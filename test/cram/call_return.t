  $ dune exec mfl -- ir fixtures/call_return.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @double_it(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    %multmp = mul i32 %x2, 2
    ret i32 %multmp
  }
  
  define i32 @quad(i32 %x) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %x2 = load i32, ptr %x1, align 4
    %calltmp = call i32 @double_it(i32 %x2)
    %calltmp3 = call i32 @double_it(i32 %calltmp)
    ret i32 %calltmp3
  }
