  $ dune exec mfl -- ir fixtures/run_shifts.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -4
  -2
  -1
  1024
  1073741824
  1073741824
  1
  65280
  16711680
