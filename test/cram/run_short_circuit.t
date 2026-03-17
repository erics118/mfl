  $ dune exec mfl -- fixtures/short_circuit.mfl > prog.ll && clang -Wno-override-module prog.ll ../../llvm/runtime.c -o prog && ./prog
  3
  4
