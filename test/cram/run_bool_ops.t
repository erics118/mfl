  $ dune exec mfl -- ir fixtures/run_bool_ops.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  false
  true
  true
  true
  true
  false
  true
  true
  false
  true
  false
  false
  true
  false
  false
  true
