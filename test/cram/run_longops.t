  $ dune exec mfl -- ir fixtures/run_longops.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -2147483648
  -1486618624
  -9999999
  -999
  -1
