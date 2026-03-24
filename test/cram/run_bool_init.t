  $ dune exec mfl -- ir fixtures/run_bool_init.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  true
  false
  true
  true
  1
  0
