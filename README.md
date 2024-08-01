# PyFunc

A functional programming language that looks a lot like Python which was built as my level-4 dissertation project at the University of Galsgow.

The language can be succinctly described as:
> A plain english expression-based impure functional programming language.

## Using PyFunc

- To use PyFunc, directly passing a source code file to the compiler executable is sufficent.
- To use the REPL, simply do not pass an argument when running the executable.
  
## Build Instructions
You build the project yourself by:
1. Installing the [requirements](#requirements).
2. Running one of the bash scripts in the "src" directory. I.e., ```bash build```.

Alternatively, you can use the *dune* build tool to build and execute the project.

**Note: You can just ignore the bash scripts if you want build the binaries manually and manipulate them further.**

## Requirements
- *dune 3.11+*
- *menhir 2.1+*
- *unionfind*
