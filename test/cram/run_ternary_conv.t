  $ dune exec mfl -- ir fixtures/run_ternary_conv.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -1
  -1
  100000
