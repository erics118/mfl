  $ dune exec mfl -- ir fixtures/func_params.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  define i32 @add(i32 %x, i32 %y) {
  entry:
    %x1 = alloca i32, align 4
    store i32 %x, ptr %x1, align 4
    %y2 = alloca i32, align 4
    store i32 %y, ptr %y2, align 4
    %x3 = load i32, ptr %x1, align 4
    %y4 = load i32, ptr %y2, align 4
    %addtmp = add i32 %x3, %y4
    ret i32 %addtmp
  }
