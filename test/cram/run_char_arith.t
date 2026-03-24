  $ dune exec mfl -- ir fixtures/run_char_arith.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -128
  200
  0
  -1
  255
  127
