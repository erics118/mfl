  $ dune exec mfl -- ir fixtures/run_casts.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  44
  -44
  -31072
  255
  65535
  0
  1
  1
  1410065408
