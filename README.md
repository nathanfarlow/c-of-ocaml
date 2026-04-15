# C_of_ocaml 🐪🤓

Compiles an OCaml program to a standalone ANSI C file. Here's a spinny cube
program running on a calculator. Its OCaml source is [here](calc/cube).

![](assets/cube.gif)

## Features
- ✅ Garbage collector
- ✅ Random selections of the stdlib
- ✅ Exceptions
- ❌ Floats
- ❌ Objects

## Usage

This branch (`oxcaml`) targets the [OxCaml](https://oxcaml.org) compiler
variant and `js_of_ocaml-compiler.6.0.1+ox`. For the original setup
(vanilla OCaml 5.2.0 + `js_of_ocaml-compiler 5.8.2`), see
`git checkout main`.

```
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
eval $(opam env --switch 5.2.0+ox)
opam install -y js_of_ocaml-compiler core core_unix ppx_jane async expect_test_helpers_async
```

To build an OCaml program for pc:
1. `git clone https://github.com/nathanfarlow/c-of-ocaml`
2. `cd c-of-ocaml`
3. `dune build examples/fib`
4. `./_build/default/examples/fib/main.c.exe`

To build for ti 84 ce:
1. Set up the [ti 84 ce toolchain](https://ce-programming.github.io/toolchain/)
2. `git clone https://github.com/nathanfarlow/c-of-ocaml`
3. `cd c-of-ocaml`
4. `dune build calc/hello_world`
5. Copy `_build/default/calc/hello_world/CAMLHI.8xp` to calculator

If you're interested, you can see an example generated C file
[here](assets/example_generated.c).
