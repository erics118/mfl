  $ dune exec mfl -- ir fixtures/nested.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  define i32 @main() {
  entry:
    %x = alloca i32, align 4
    store i32 3, ptr %x, align 4
    ret i32 0
  }
