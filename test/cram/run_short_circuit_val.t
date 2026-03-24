  $ dune exec mfl -- ir fixtures/run_short_circuit_val.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  1
  1
  0
  1
  0
  1
