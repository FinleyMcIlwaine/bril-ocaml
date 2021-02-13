# Bril OCaml
A simple OCaml library for dealing with the "Big Red Intermediate Language",
an educational IR used in Cornell's advanced compilers course CS6120.

## Why

> The [Bril GitHub](https://github.com/sampsyo/bril) contains an OCaml
library already, why did you make this one?

The existing OCaml library has "too many" features. When it loads up a Bril
program, it constructs the basic blocks for you. Moreover, it implements a 
lot of the features that a new student would like to implement on their own.

This library does nothing but load up Bril programs into a "raw" AST-like
data structure, and that's it.

## Usage

You can pin this library with opam:
```
opam pin add bril https://github.com/FinleyMcIlwaine/bril-ocaml.git
```

Then include it in your Dune files as `bril` and use it just as you would
any other dependency!

The main function is the `Bril.from_json` function, which loads a Bril
program in its JSON representation into the AST provided by this library.
From there, you can follow along with CS6120 and implement all the cool
compiler stuff on your own!
