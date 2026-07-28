# Mangrove

Mangrove is a library for building command line argument parsers using
Haskell's `Applicative` interface. It provides parsers for UNIX-style
command line syntax, including positional parameters, named options,
and commands, as well as complex subparameters and suboptions (e.g.
`--mount read-only,src=/dev/sda1,dst=/data`). It is also extensible,
so you can define alternative command line syntaxes.

## Building

This project uses `stack` as its primary build system (though building
with `cabal` should also work). As usual, you can build the library
by running `stack build` and you can install it with `stack install`.
Use `stack haddock` to build the API documentation.

## Test Suite

Mangrove has a test suite using HSpec. Run `stack test` to build and
run the tests. The `main` branch should always pass all tests, so if
something fails, please make an issue!

# Tutorial

## A Metaphor

Imagine the roots of a plant branching out like a tree as they
descend. Eventually, they dip into a stream. The roots collect water
and nutrients from the flowing stream. These resources travel back up
the structure toward the plant, combining along the way.

This is kind of like how the Mangrove library works. We build a
tree-shaped parser from simple applicative combinators then feed it a
sequence of CLI arguments. Simple parsers stationed at the bottom of
the tree consume these arguments and produce values which are then
passed back up the tree and combined with the results of other parsers
until a final result is reached.

## Example

__NOTE__: See the full example file in `doc/MkUser.hs`.

Suppose we are writing a simple program that creates new user
accounts - we'll call it "mkuser". The goal will be to provide a
command line interface with the following syntax:

```
mkuser [--uid=INT] [--system] [--groups={GROUP...}] USERNAME
```

First, let's create a new record that captures the program's runtime
configuration.

```haskell
data Settings = Settings
  { userId     :: Maybe Int -- ^ An optional target user ID
  , userSystem :: Bool -- ^ Is this a system user?
  , userGroups :: [Text] -- ^ Groups the new user will be in
  , userName   :: Text  -- ^ Username for the new user
  } deriving (Show)
```

Let's also pretend that our program's logic lives inside a function
`run :: Settings -> IO ()`. We pass it the settings we want, and it
runs the program accordingly. However, since this is just an example
program, we won't actually create any user accounts; instead we'll
just have the program print its settings to `stdout`.

```haskell
run :: Settings -> IO ()
run = print
```

Now we need to construct a parser that reads a list of arguments and
yields a `Settings`. Our parser will have the type `UnixParser
Settings`.

__NOTE__: This example uses the language extensions `OverloadedLists`
and `OverloadedStrings` since we need to write lots of `NonEmpty` list
and `Text` literals.

__NOTE__: `UnixParser` is just a convenient type synonym for
`ParseTree UnixScheme`. This tells us that we will build a `ParseTree`
by combining parsers from the Unix scheme.

## Parameters

A "parameter" is a positional input that accepts the first non-flag
argument it encounters. For example, consider a program called
`substring` whose command line syntax is `substring START END STRING`.
`START`, `END`, and `STRING` are parameters. If we invoke `substring 1
3 "example"`, we know that `START` is `1`, `END` is `3`, and `STRING`
is `"example"` because of the order in which they appear.

Our program will have just one parameter: a username. Here is how we
define a parser for it:

```haskell
prm_name :: UnixParser Text
prm_name = parameter defaultParser
```

The `parameter` function creates a parameter parser out of a
`TextParser` (see below).

### TextParsers

A `TextParser r` is just a wrapper around a function that parses
`Text` into a value of type `r`. It also contains a "hint" string used
for displaying usage information.

Many common data types have a reasonable default `TextParser`
implementation. Types that are instances of the `DefaultParser` class
implement `defaultParser :: DefaultParser a => TextParser a`, letting
us automatically select the correct parser based on the required type.

In the example above, `Text` has a very simple `DefaultParser`
instance that just returns its input unchanged.

## Options

An "option" is a construct representing a named input. Options begin
with a flag followed by an optional subargument string.

A "flag" is special symbol that signals the beginning of a particular
option. Per UNIX tradition there are long flags (e.g. `--foo`) and
short flags (e.g `-f`).

To prevent ambiguity, sometimes an equals sign is used to separate a
long flag from its subargument string (instead of a space). For
example, `--uid=1000` is an option that begins with the `--uid` flag
and is followed by the subargument string `1000`. Similarly, an
option's short flag can be directly concatenated with its argument,
e.g. `-u 1000` can be written `-u1000`.

Let's define a parser for the `--uid` option:

```haskell
opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"]
          "Specify a user ID"
		  $ subparameter defaultParser
```

The `option` function creates a parser for CLI options. It takes three
arguments:

1. A `NonEmpty` list of `Flag`s that trigger the option, in this case
   "--uid" and "-u". `Flag` is an instance of `IsString`, so we can
   just write the string representation instead of `LongFlag "uid"`
   and `ShortFlag 'u'`.
2. A human readable description. This will be displayed when help
   output is triggered.
3. A subparser tree (`SubParser r`) that will parse any subparameters or
   suboptions. In this case, we declare a single subparameter (an
   integer).

The `subparameter` function behaves just like `parameter` from
earlier, except it creates a `SubParser` instead of a `UnixParser`. We
also use `defaultParser` to automatically select an appropriate
`TextParser` for `Int`.

__NOTE__: `SubParser` is a type synonym for `ParseTree SubScheme`.
That means we build a `SubParser` by combining `SubScheme` parsers.
`SubScheme` provides parsers for handling subarguments to options.

You might notice that our `Settings` record requires a `Maybe Int`,
not an `Int`. However, since `UnixParser` is an instance of
`Alternative`, we can use `optional` from `Control.Applicative`.
`optional opt_uid :: UnixParser (Maybe Int)` describes an option that
is not required and might be absent (which should give us `Nothing`).

### Switches

The `--system` option is simpler because because it doesn't accept any
subarguments - it is either present (`True`) or absent (`False`). This
special type of option is a "switch", and we can use the `switch`
function to create a parser:

```haskell
opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"

-- If we defined this without 'switch' it would look like this:
-- opt_system = option ["--system", "-s"]
--              "Create a system user"
--              (pure True)
--              <|> pure False

```

### Options with Multiple Subparameters

Let's deal with the `--groups` option. This option is a bit different
from the `--uid` option because we want the user to be able to specify
a list of groups for the new user to join. Thus, we want to create an
option that accepts one or more subarguments.

Thankfully, `SubParser` is also an `Alternative` instance. We can use
`some` (from `Control.Applicative`) to convert a `SubParser r` into a
`SubParser [r]` that will expect to parse one or more `r` values.

```haskell
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser
```

Mangrove recognizes that the subparser `some $ subparameter
defaultParser :: SubParser [Text]` can consume multiple subarguments,
so it splits those subarguments apart by comma. This allows us to pass
a list of group names like so: `--groups=wheel,audio,input`, and the
parser will yield `["wheel","audio","input"]`.

By using `some` instead of the similar function `many`, we have
created a subparser that will fail if no subarguments are provided
(e.g. `mkuser alice --groups`).

What if the `--groups` option isn't present at all? We still need a
`[Text]` value for our `Settings` record. In that case, an empty list
makes sense. Just like with our `--uid` option, we use `Alternative`
to define what happens if our parser never finds applicable input.

```
opt_groups <|> pure [] :: UnixParser [Text]
```

__NOTE__: There is an important distinction between a parser that
never finds relevant input and a parser that fails. In an expression
like `opt_groups <|> pure []`, if `opt_groups` never finds relevant
input, the alternative provides a default value. However, if
`opt_groups` *does* find applicable input, but parsing it fails, an
error will be thrown instead.

More generally, if `p` and `q` are parsers, then `p <|> q` is a parser
that yields the result from whichever parser finds applicable input
first. If neither parser finds input, we first try resolving `p` and
then `q` with no input and yield the first result we get. If neither
succeeds, we throw an error.

## Applicative

We are now ready to construct our `Settings` parser using `<$>` and
`<*>`:

```haskell
parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name
```

Now we can inspect the automatically generated usage information for
our parser in GHCi using `render` from `Mangrove.Text`:

```
ghci> render parseSettings
"[--uid=INT] [--system] [--groups={STRING...}] STRING"
```

This output indicates that our parser accepts (but does not require) a
`--uid` option with an integer subargument, a `--system` option, and a
`--groups` option with a list of string subarguments. Finally, it
requires a single parameter, which is a string. We'll see how to
improve those type hints later.

## Running the Parser

The `parseArguments` function will run our parser with the arguments
passed to our program by the operating system.

```haskell
main :: IO ()
main = parseArguments parseSettings "mkuser" "Create user accounts" run
```

`parseArguments` takes four arguments: a `UnixParser r`, the name of
the program (for help output), a description of the program (also for
help output), and a function of type `r -> IO a`. When the parser
completes successfully, this function will be called with the result.
Otherwise, `parseArguments` will print error messages or help
information as appropriate.

If you want to run an argument parser without using `IO`, or you want
to pass your own argument list, check out `runArgumentParser` from
`Mangrove.ArgumentParser`.

Now we have a complete program we can build and run to show the
argument parser in action!

```
$ ghc -o mkuser MkUser.hs
[1 of 2] Compiling Main             ( MkUser.hs, MkUser.o )
[2 of 2] Linking mkuser

$ ./mkuser --system --groups audio,input bilbo
Settings {userId = Nothing, userSystem = True, userGroups = ["audio","input"], userName = "bilbo"}

$ ./mkuser --badinput
unexpected --badinput

$ ./mkuser --system
expected: STRING

$ ./mkuser --uid=InvalidNumber bilbo
--uid=InvalidNumber: InvalidNumber: input does not start with a digit
```

## Help Options

Currently, our CLI interface is missing something important: an option
for displaying help and usage information. Let's create a new
`Settings` parser that recognizes `--help` as a request for help
information.

```haskell
parseSettings' :: UnixParser Settings
parseSettings' = addHelpOptions ["--help"]
                 "Display help and usage information"
                 parseSettings

main :: IO ()
main = parseArguments parseSettings' "mkuser" "Create user accounts" run
```

Now if we invoke our program with the `--help` option, it will display
a nice summary of how to use it:

```
./mkuser --help
Usage:
mkuser [--uid=INT] [--system] [--groups={STRING...}] STRING
mkuser --help

Create user accounts

    --help                 Display help and usage information
-g  --groups  {STRING...}  Specify what groups the user is part of
-s  --system               Create a system user
-u  --uid     INT          Specify a user ID
```

__NOTE__: If an interface defines any commands (see below),
`addHelpOptions` will add a help option at the root of the parse tree
as well as the root of every command subtree. This is so that you can
invoke `myprogram --help` to get general help or `myprogram
somecommand --help` to get help information specifically for
`somecommand`.

## Hints

Type hints are displayed as placeholders for parameters in help and
usage information. They are a hint to the user about what kind of
information is expected by that input. For example, `--uid=INT`
indicates the `--uid` option expects an integer as a subargument.
Hints stored inside the `parserHint` field of a `TextParser`.

Our program uses the generic hints defined in the `DefaultParser`
instances for `Int` and `Text`. These defaults are often reasonable,
but we can also tailor hints more specifically for our use case. All
we need to do is alter the value of `parserHint` for the relevant
`TextParser`.

```
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser {parserHint = "GROUP"}

prm_name :: UnixParser Text
prm_name = parameter defaultParser {parserHint = "USERNAME"}
```

Now our help output looks like this:

```
$ ./mkuser --help
Usage:
mkuser [--uid=INT] [--system] [--groups={GROUP...}] USERNAME
mkuser --help

Create user accounts

    --help                Display help and usage information
-g  --groups  {GROUP...}  Specify what groups the user is part of
-s  --system              Create a system user
-u  --uid     INT         Specify a user ID
```

## Commands

A "command" is a special argument changes the context of a parser.
When a command is encountered, the parser begins using the parse tree
associated with that command as a new context until it completes.
Commands are usually used as a way to invoke different modes of
functionality for a single program. For example, `git` supports
various commands like `commit` or `pull`.

Suppose we are creating a basic version control system similar to
`git`. Our program will have several runtime modes for doing
operations like `commit` or `pull`. Here is how we might define a
parser that recognizes the corresponding commands (for the full code,
see `doc/VersionControl.hs`):

```haskell
data Mode
  = CommitMode CommitSettings
  | PullMode PullSettings
  -- ... and probably other modes too
  deriving (Show)

parseMode :: UnixParser Mode
parseMode = cmd_commit <|> cmd_pull
  where
    cmd_commit =
      command ["commit"]
      "Make a new commit"
      $ CommitMode <$> parseCommitSettings
    cmd_pull =
      command ["pull"]
      "Download remote changes"
      $ PullMode <$> parsePullSettings
```
