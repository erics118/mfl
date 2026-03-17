  $ dune exec mfl -- fixtures/div_zero_guard.mfl > prog.ll && clang -Wno-override-module prog.ll ../../llvm/runtime.c -o prog && ./prog
  true
  false
