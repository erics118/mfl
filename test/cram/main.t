  $ dune exec mfl -- fixtures/main.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  declare void @printint(i32)
  
  declare void @printbool(i1)
  
  define i32 @main() {
  entry:
    ret i32 0
  }
