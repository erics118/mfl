  $ dune exec mfl -- ir fixtures/full.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printbool(i1 zeroext)
  
  declare void @printint(i32)
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 5, ptr %x, align 4
    %x1 = load i32, ptr %x, align 4
    %eqtmp = icmp eq i32 %x1, 3
    call void @printbool(i1 zeroext %eqtmp)
    %x2 = load i32, ptr %x, align 4
    %eqtmp3 = icmp eq i32 %x2, 3
    br i1 %eqtmp3, label %then, label %else
  
  then:                                             ; preds = %entry
    %x4 = load i32, ptr %x, align 4
    call void @printint(i32 %x4)
    br label %merge
  
  else:                                             ; preds = %entry
    %x5 = load i32, ptr %x, align 4
    %subtmp = sub i32 %x5, 1
    call void @printint(i32 %subtmp)
    br label %merge
  
  merge:                                            ; preds = %else, %then
    ret i32 0
  }
