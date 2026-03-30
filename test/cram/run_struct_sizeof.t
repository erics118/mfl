  $ dune exec mfl -- ir fixtures/struct_sizeof.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  8
  12
