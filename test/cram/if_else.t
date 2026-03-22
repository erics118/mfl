  $ dune exec mfl -- ir fixtures/if.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 5, ptr %x, align 4
    %x1 = load i32, ptr %x, align 4
    %gttmp = icmp sgt i32 %x1, 3
    br i1 %gttmp, label %then, label %else
  
  then:                                             ; preds = %entry
    %x2 = load i32, ptr %x, align 4
    call void @printint(i32 %x2)
    br label %merge
  
  else:                                             ; preds = %entry
    call void @printint(i32 0)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    ret i32 0
  }
