  $ dune exec mfl -- ir fixtures/run_incdec_vals.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  5
  6
  6
  6
  3
  2
  2
  2
