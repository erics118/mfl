  $ dune exec mfl -- ir fixtures/run_ptr_scaling.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  1
  2
  1
