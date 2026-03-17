  $ dune exec mfl -- fixtures/print.mfl > prog.ll && clang -Wno-override-module prog.ll ../../llvm/runtime.c -o prog && ./prog
  6
