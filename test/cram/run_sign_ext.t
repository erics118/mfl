  $ dune exec mfl -- ir fixtures/run_sign_ext.mfl > prog.ll && clang -Wno-override-module prog.ll ../../runtime/runtime.c -o prog && ./prog
  -1
  255
  -1
  65535
