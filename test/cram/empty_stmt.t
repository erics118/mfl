  $ dune exec mfl -- ir fixtures/empty_stmt.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i8)
  
  define i32 @main() {
  entry:
    ret i32 0
  }
