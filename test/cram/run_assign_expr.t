  $ dune exec mfl -- ir fixtures/run_assign_expr.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  42
  42
  10
  10
  0
  0
