# mfl

subset of C implemented in OCaml, compiled to LLVM IR

## features

**types**
- [x] `int`, `bool`, `void`
- [ ] `char`, `short`, `long`, `long long`, unsigned variants
- [ ] `float`, `double`
- [ ] pointer types: `int *p`
- [ ] array types: `int a[10]`
- [ ] `struct`, `union`
- [ ] `enum`
- [ ] `typedef`
- [ ] function pointer types

**operators**
- [x] arithmetic: `+`, `-`, `*`, `/`, `%`
- [x] bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- [x] comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- [x] logical: `&&`, `||`, `!`
- [x] unary negation: `-`
- [x] operator precedence and associativity
- [x] ternary: `?:`
- [ ] compound assignment: `+=`, `-=`, `*=`, `/=`, `&=`, `|=`, `^=`, `<<=`, `>>=`
- [x] increment/decrement: `++`, `--` (pre and post)
- [ ] `sizeof`
- [ ] comma operator

**expressions**
- [x] integer and boolean literals
- [x] variable references
- [x] assignment as expression (`int z = (x = 3)`)
- [x] function calls
- [ ] cast expressions: `(int)x`
- [ ] address-of `&x` and dereference `*p`
- [ ] subscript `a[i]`
- [ ] member access `s.x`, `s->x`
- [ ] char literals: `'a'`
- [ ] string literals: `"hello"`
- [ ] adjacent string literal concatenation: `"foo" "bar"`
- [ ] string escape sequences: `\n`, `\t`, `\0`, `\\`, `\"`, `\xNN`
- [ ] hex/octal/float literals: `0xFF`, `0777`, `3.14`
- [ ] integer suffixes: `42UL`, `1LL`

**statements**
- [x] variable declarations with initializer: `int x = 0`
- [x] `if`/`else`
- [x] `while`, `for`
- [x] `return`
- [x] blocks / compound statements
- [x] `break`, `continue`
- [ ] `do`/`while`
- [ ] `switch`/`case`/`default`
- [ ] `goto` and labels
- [x] `for` with optional init/cond/incr (e.g. `for (;;)`)
- [x] uninitialized declarations: `int x;`
- [ ] multiple declarators: `int x = 0, y = 1;`

**functions**
- [x] definitions and calls with parameters
- [x] `int`, `bool`, `void` return types
- [x] implicit `return 0` for `main`, implicit `ret void` for void functions
- [ ] forward declarations: `int foo(int x);`
- [ ] `extern` declarations
- [ ] variadic functions: `va_list`, `va_start`, `va_end`
- [ ] `(void)` parameter list: `void foo(void)` vs `void foo()`

**scoping**
- [x] function scope
- [x] block scoping
- [ ] global variables
- [ ] `static` local variables
- [ ] `static`, `const`, `extern`, `volatile` qualifiers

**types**
- [ ] array-to-pointer decay: `int a[10]; int *p = a`
- [ ] integer promotion and implicit conversion rules
- [ ] `<stddef.h>` types: `size_t`, `ptrdiff_t`, `offsetof`
- [ ] `<stdint.h>` types: `int32_t`, `uint64_t`, etc.

**comments**
- [x] `//` line comments
- [x] `/* */` block comments

**preprocessor**
- [ ] `#include`
- [ ] `#define` / `#undef` — object-like and function-like macros
- [ ] `#` stringification and `##` token pasting
- [ ] variadic macros: `#define log(...) __VA_ARGS__`
- [ ] `#ifdef` / `#ifndef` / `#if` / `#elif` / `#else` / `#endif`
- [ ] `defined()` operator
- [ ] `#pragma`
- [ ] `#error`, `#line`
- [ ] predefined macros: `__FILE__`, `__LINE__`, `__func__`, `__DATE__`, `__TIME__`
- [ ] multiline macros with `\` continuation

**other**
- [ ] `NULL`

## usage

```sh
# emit LLVM IR
dune exec mfl -- ir program.mfl

# pretty-print source
dune exec mfl -- format program.mfl

# link with runtime and produce a binary
dune exec mfl -- ir program.mfl | clang -Wno-override-module -x ir - runtime/runtime.c -o program

# run
./program
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

gadt:
- https://ocaml.org/manual/5.4/gadts-tutorial.html

possible code resources:
- https://github.com/arbipher/llvm-ocaml-tutorial/
- https://github.com/adamrk/llvm-ocaml-tutorial
