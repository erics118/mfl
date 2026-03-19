  $ dune exec mfl -- ir fixtures/full.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  false
  4
