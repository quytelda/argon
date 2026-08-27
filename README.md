# Mangrove (mangrove-cli)

[![Hackage](https://img.shields.io/hackage/v/mangrove-cli.svg)](https://hackage.haskell.org/package/mangrove-cli)
[![Unit Tests](https://github.com/quytelda/mangrove/actions/workflows/unit-tests.yml/badge.svg)](https://github.com/quytelda/mangrove/actions/workflows/unit-tests.yml)

Mangrove is a library for building command line argument parsers using
Haskell's `Applicative` interface. It provides parsers for UNIX-style
command line syntax, including positional parameters, named options,
and commands, as well as complex subparameters and suboptions (e.g.
`--mount src=/webroot,dst=/var/www,rw`). It is also extensible, so you
can define alternative command line syntaxes.

## Documentation

The API documentation is available on Hackage:
<https://hackage.haskell.org/package/mangrove-cli>

There is a tutorial to help with getting started:
<https://github.com/quytelda/mangrove/wiki/Tutorial>

Commands are covered in a separate tutorial:
<https://github.com/quytelda/mangrove/wiki/Commands>

## Obtaining

Mangrove is available on Hackage as `mangrove-cli`:
<https://hackage.haskell.org/package/mangrove-cli>

__NOTE__: This project is not related to the `mangrove` package on
Hackage. Make sure to use the package name `mangrove-cli`.

The source code for Mangrove is hosted on GitHub:
<https://github.com/quytelda/mangrove>

## Building

This project uses `stack` as its primary build system (though building
with `cabal` should also work). As usual, you can build the library
by running `stack build` and you can install it with `stack install`.
Use `stack haddock` to build the API documentation.

## Test Suite

Mangrove has a test suite using HSpec. Run `stack test` to build and
run the tests. The `main` branch should always pass all tests, so if
something fails, please make an issue!

## Project Roadmap

This project is currently under active development. Goals currently on
the horizon include:

- Stabilize the client-facing API
- Simplify the code structure
- Improve test suite coverage
- Profiling & optimization

Once these are addressed, a 1.0.0 release will be appropriate.
