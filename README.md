# mfl

c implemented in ocaml using llvm from scratch

## how to run

```sh
# generate ll file
dune exec mfl "1 + 2"

# execute
lli output.ll

# alternatively, compile to native code
clang output.ll -o output && ./output
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

