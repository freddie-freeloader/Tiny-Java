[![Build Status](https://travis-ci.org/freddie-freeloader/Tiny-Java.svg?branch=master)](https://travis-ci.org/freddie-freeloader/Tiny-Java)

# Tiny-Java

A Compiler for a subset of Java :coffee:

## tl;dr

Use the [stack build tool](https://docs.haskellstack.org/en/stable/README/) to
install the compiler locally (execute `stack install` in the root directory).

## How to install/build/test

- Install the [stack build tool](https://docs.haskellstack.org/en/stable/README/)
- Go into the root directory of this project and
  - use `stack build` to build the project

    You may also use the flag `--file-watch`. That way `stack` recompiles the files in `src`
    every time you change a file.
  - use `stack install` to install a compiled binary of this compiler locally

    The program should be in your `$PATH` with the name `tiny-java`. Use e.g.
    `tiny-java foo.java` to compile a file. `tiny-java --help` should you
    provide with some more information on how to use it.
  - use `stack test` to let stack run all tests in the `test` directory

    We use [hspec](http://hspec.github.io/) for our test suite.
  - use `stack repl` to build the project and load it into a `ghci`-session

    In the `ghci`-session you can use `:reload` (shorthand `:r`) to trigger a rebuild of the project and load
    the new build in the session.
  - use `stack haddock` to generate html-documentation in `/docs`

    The documentation is then available [here](https://freddie-freeloader.github.io/Tiny-Java/) (after pushing the updated
    `docs`-directory).
## [Documentation](https://freddie-freeloader.github.io/Tiny-Java/)
