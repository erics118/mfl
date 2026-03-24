  $ dune exec mfl -- ir fixtures/run_bool_incdec.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  true
  true
  false
  true
