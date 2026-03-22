  $ dune exec mfl -- ir ../../examples/pointers.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  42
  43
  99
  1
  100
