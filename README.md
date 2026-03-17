# mfl

subset of c implemented in ocaml compiled to LLVM IR, written in ocaml

## features

- [x] integers, booleans, arithmetic, bitwise, comparison, logical operators
- [x] operator precedence and associativity
- [x] interpreter with type checking (old, now it's a compiler)
- [x] statements and blocks
- [x] variable declarations and assignment of `int` and `bool` types
- [x] `if`/`else`
- [x] `while` and `for` loops
- [x] `return`
- [x] function definitions and calls (`int`, `bool`, `void` return types)
- [x] ternary expressions

## usage

```sh
# compile to IR
dune exec mfl -- program.mfl > program.ll

# link with runtime and produce a binary
clang llvm/runtime.c program.ll -o program

# run
./program
```

or as a one-liner:

```sh
dune exec mfl -- program.mfl | clang llvm/runtime.c -x ir - -o program && ./program
```

## examples

located under `examples/`

- `factorial.mfl`: prints the factorial of 10
- `fibonacci.mfl`: prints the first 10 Fibonacci numbers
- `primes.mfl`: prints the primes under 100

## testing

```sh
# run tests
dune test

# run test with coverage
dune test --instrument-with bisect_ppx --force
```

## sources

initial code is based off of, but written in ocaml instead of haskell: [Daniel J. Harvey's blog](https://danieljharvey.github.io/llvm-compiler-part-1/) 

resources:
- https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html
- https://blog.josephmorag.com/posts/mcc1/
- https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/

possible code resources:
- https://github.com/arbipher/llvm-ocaml-tutorial/
- https://github.com/adamrk/llvm-ocaml-tutorial
