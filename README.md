# mfl

c implemented in ocaml using llvm from scratch

## features

- [x] lexer for integers, booleans, parens, unary and binary operators
- [x] parser for basic integer and boolean expressions
- [x] handle operator predecence and associativity correctly
- [x] interpreter with type checking
- [x] support for statements with `;`
- [x] add statement blocks blocks with  `{}`
- [ ] `int` variable declarations and assignment
- [ ] `bool`/`_Bool` variable declarations and assignment
- [ ] `if`/`else` control flow
- [ ] `while` loops
- [ ] variable scoping
- [ ] `return` statements
- [ ] basic function definitions and calls
- [ ] function definitions with parameters and different return types


## how to run

currently, it returns the value of the last expression in the input

```sh
# evaluate an expression
dune exec mfl -- "1 + 2 * 3;" # 7
dune exec mfl -- "1 + 2; 4 + 5;" # 9
dune exec mfl -- "1 <= 3;" # true
dune exec mfl -- "1 * 8 >= 2 && 1 / 0;" # true, short circuits and doesn't error
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
