  $ dune exec mfl -- ir fixtures/void_func.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  42
