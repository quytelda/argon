# Changelog for `mangrove`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.3.0.0 - 2026-08-27

### Added

- Request options that trigger a request when matched
- Literate Haskell mkuser tutorial that demonstrates how to build
  parsers for options and parameters
- Literate Haskell pkgtool tutorial that demonstrates how to build
  command parsers
- Individual text parsers for common data types

### Changed

- Generalize help requests to help or version requests
- Rewrite mkuser tutorial as a Literate Haskell program
- Add version information to ProgramInfo
- Move tutorials from the inside the README to dedicated pages on the
  GitHub wiki which are built from Literate Haskell files in the `doc`
  directory

### Fixed

- Include commands with no options in help output
- Factor out duplicate code in Mangrove.Unix.optionPure

### Removed

- Separable module and Separable typeclass
- Exhibit type
- Modal type

## 0.2.0.0 - 2026-08-14

### Added

- Documentation about where to obtain this library

### Changed

- Loosen bounds on dependency versions so the project can be built
  against a wider range of snapshots
- Change the error message type for TextParsers from 'Builder' to 'Text'
- Fix missing help options in help output
- Fix unit test builds when using cabal
- Make minor code quality improvements
- Add strictness annotations to simple values in constructors

## 0.1.0.0 - 2026-08-12

### Added

- Types and data structures for building argument parsers
- Combinators for constructing parsers
- Typeclass for parsing schemes (Scheme)
- A UNIX-style parsing scheme with a subargument parsing scheme
- A stream parsing monad (StreamParser)
- Support for generating help information
- A basic test suite with 53 unit tests
- An API for running parsers and collecting the results
