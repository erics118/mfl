  $ dune exec mfl -- ir fixtures/div_zero_guard.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  true
  false
