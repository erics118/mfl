  $ dune exec mfl -- ir fixtures/run_incdec.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  11
  11
  12
  4
  4
  3
