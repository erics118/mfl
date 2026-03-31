  $ dune exec mfl -- ir fixtures/vars.mfl
  ; ModuleID = 'mfl'
  source_filename = "mfl"
  
  define i32 @main() {
  entry:
    %a = alloca i32, align 4
    store i32 8, ptr %a, align 4
    %b = alloca i32, align 4
    store i32 10, ptr %b, align 4
    %c = alloca i8, align 1
    store i8 1, ptr %c, align 1
    %z = alloca i32, align 4
    store i32 1, ptr %b, align 4
    store i32 1, ptr %z, align 4
    %z1 = load i32, ptr %z, align 4
    ret i32 %z1
  }
