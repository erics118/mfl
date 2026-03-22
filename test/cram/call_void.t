  $ dune exec mfl -- ir fixtures/print.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @main() {
  entry:
    call void @printint(i32 6)
    ret i32 0
  }
