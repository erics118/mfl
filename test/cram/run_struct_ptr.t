  $ dune exec mfl -- ir fixtures/struct_ptr.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  3
  4
  100
