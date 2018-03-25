# A minimal Lisp-like language interpreter in Haskell

A course project of *Functional Programming* at Tsinghua University in the fall semester of 2016. 

## Features

* Purely functional
* Number, Boolean, pair types and built-in functions.
* Lexical scope (`let` assignment)
* Fully functional lambda calculus

## Non-features :(

* No control flow with native Boolean type. (Use [Church encoding](https://en.wikipedia.org/wiki/Church_encoding), though you have to re-invent everything and you won't get neat output).
* No recursion. (Use [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator) if you need recursion)

## Usage

```shell
cd src
cabal build
dist/build/ki -i FILE # Interpret FILE and write result to stdout
dist/build/ki --repl # Open in REPL mode
```

See `docs/README.md` (Chinese) for details.

See `examples` for sample programs.

## Technical Details 

See `docs/report.md` (Chinese).