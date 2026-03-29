  $ dune exec mfl -- ir fixtures/run_sizeof.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  4
  1
  1
  8
  12
  4
  1
  1
  8
