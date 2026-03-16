# mfl

c-like language implemented in ocaml from scratch

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
- [ ] `while` loops
- [ ] variable scoping


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

resources:
- https://danieljharvey.github.io/llvm-compiler-part-1/
- https://blog.josephmorag.com/posts/mcc1/
- http://troydm.github.io/blog/2014/03/29/writing-micro-compiler-in-ocaml/
