  $ dune exec mfl -- ir ../../examples/fibonacci.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34
