# Tiny-Java

A Compiler for a subset of Java :coffee:

## How to build/test

- Install the [stack build tool](https://docs.haskellstack.org/en/stable/README/)
- Go into the root of this project and
  - use `stack build` to build the project
  
    You may also use the flag `--file-watch`. That way **stack** recompiles the files in `src`
    every time you change a file.
  - use `stack test` to let stack run all tests in the `test` directory
  
    We use [hspec](http://hspec.github.io/) for our test suite.
  - use `stack haddock` to generate html-documentation in `/docs`
  
    The documentation is then available [here](https://freddie-freeloader.github.io/Tiny-Java/) (after pushing the updated
    `docs`-directory).
## [Documentation](https://freddie-freeloader.github.io/Tiny-Java/)

