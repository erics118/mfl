# mfl

c implemented in ocaml using llvm from scratch

## features

- [x] lexer for integers, booleans, parens, unary and binary operators
- [x] parser for basic integer and boolean expressions
- [x] handle operator predecence and associativity correctly
- [x] interpreter with type checking
- [x] support for statements with `;`
- [x] add statement blocks blocks with  `{}`
- [x] `int` variable declarations and assignment
- [x] `bool`/`_Bool` variable declarations and assignment
- [x] `if`/`else` control flow
- [x] `return` statements
- [x] basic function definitions and calls
- [x] function definitions with parameters and different return types
- [x] `for loops` loops
- [x] `while` loops



## how to run

the compiler emits LLVM IR to stdout. compile and link with `clang` and the runtime:

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

## sources

initial code is based off of, but written in ocaml instead of haskell: [Daniel J. Harvey's blog](https://danieljharvey.github.io/llvm-compiler-part-1/) 

resources:
- https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html
- https://blog.josephmorag.com/posts/mcc1/  
- https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/

possible code resources:
- https://github.com/arbipher/llvm-ocaml-tutorial/
- https://github.com/adamrk/llvm-ocaml-tutorial
