  $ dune exec mfl -- ir fixtures/run_unsigned_arith.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  0
  -1
  0
  33
  1
