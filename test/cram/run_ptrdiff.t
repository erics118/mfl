  $ dune exec mfl -- ir fixtures/run_ptrdiff.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  3
  7
  2
  1
