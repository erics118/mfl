  $ dune exec mfl -- ir fixtures/run_int_promotion.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  200
  400
  40000
