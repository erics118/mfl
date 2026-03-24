  $ dune exec mfl -- ir fixtures/run_long_shift.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -4
  1073741824
