  $ dune exec mfl -- ir ../../examples/factorial.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  3628800

