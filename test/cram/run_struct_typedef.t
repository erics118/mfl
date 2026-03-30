  $ dune exec mfl -- ir fixtures/struct_typedef.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  7
  8
