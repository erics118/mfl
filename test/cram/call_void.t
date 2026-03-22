  $ dune exec mfl -- ir fixtures/print.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1 zeroext)
  
  define i32 @main() {
  entry:
    call void @printint(i32 6)
    ret i32 0
  }
