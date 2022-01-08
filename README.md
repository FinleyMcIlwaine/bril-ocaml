# Bril OCaml
A simple OCaml library for dealing with the "Big Red Intermediate Language",
an educational IR used in Cornell's advanced compilers course CS6120.

## Why

> The [Bril GitHub](https://github.com/sampsyo/bril) contains an OCaml library already, why did you make this one?

The existing OCaml library has "too many" features. When it loads up a Bril
program, it constructs the basic blocks for you. Moreover, it implements a 
lot of the features that a new student would like to implement on their own.

This library does nothing but load up Bril programs into a "raw" AST-like
data structure, and that's it.

## Usage

You can pin this library with opam:
```
opam pin add bril git://github.com/FinleyMcIlwaine/bril-ocaml#core
```

The command above will pin the `core` branch of this repo, which only has
support for the "core" Bril language. If you would like to have types for
the extensions mentioned in the official repository (memory, floating
points, and speculative execution), you can run:

```
opam pin add bril git://github.com/FinleyMcIlwaine/bril-ocaml#extended
```

That command will pin the `extended` branch of this repo that has support
for those features.

Once you've pinned it, you can include it in your Dune files as `bril` and
use it just as you would any other dependency!

The main function is the `Bril.from_json` function, which loads a Bril
program in its JSON representation into the AST provided by this library.
From there, you can follow along with CS6120 and implement all the cool
compiler stuff on your own!
