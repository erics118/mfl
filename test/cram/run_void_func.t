  $ dune exec mfl -- fixtures/void_func.mfl > prog.ll && clang -Wno-override-module prog.ll ../../llvm/runtime.c -o prog && ./prog
  42
