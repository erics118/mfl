  $ dune exec mfl -- ir fixtures/run_divmod.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -3
  -3
  3
  -1
  1
  -1
  10
  0
