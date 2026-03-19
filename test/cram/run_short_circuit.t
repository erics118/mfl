  $ dune exec mfl -- ir fixtures/short_circuit.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  3
  4
