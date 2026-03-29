  $ dune exec mfl -- ir ../../examples/quicksort.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  0
  1
  2
  3
  4
  5
  6
  7
  8
  9
