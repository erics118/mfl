  $ dune exec mfl -- ir fixtures/struct_basic.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  10
  20
