  $ dune exec mfl -- fixtures/full.mfl > prog.ll && clang -Wno-override-module prog.ll ../../llvm/runtime.c -o prog && ./prog
  false
  4
