  $ dune exec mfl -- ir fixtures/print.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  6
