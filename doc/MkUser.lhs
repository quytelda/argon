Tutorial
========

Suppose we are writing a simple program that creates new user
accounts - we'll call it "mkuser". The goal will be to provide a
command line interface with the following syntax:

```
mkuser [--uid=INT] [--system] [--groups={GROUP...}] USERNAME
```

This file is a literate Haskell program. That means it also is a valid
Haskell program that you can compile:

```
$ stack exec ghc -- -o mkuser doc/MkUser.lhs
[1 of 2] Compiling Main             ( doc/MkUser.lhs, doc/MkUser.o )
[2 of 2] Linking mkuser
```

Getting Started
---------------

First of all, we will need to write lots of `NonEmpty` list and `Text`
literals, so we'll enable some language extensions to make that
easier.

\begin{code}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

Now let's import the modules we need. The building blocks for our
parser are found in `Mangrove.Unix` while the functions for running
parsers live in the `Mangrove` module. We'll use combinators from
`Control.Applicative` to construct our parser. We'll also import
`Text`, since Mangrove is built around `Text` rather than `String`.
Finally, we'll use `Data.Version` when specifying program metadata.

\begin{code}
import           Control.Applicative
import           Data.Text           (Text)
import           Mangrove
import           Mangrove.Unix
import           Data.Version
\end{code}

Next let's create a new record that captures the program's runtime
configuration:

\begin{code}
-- | This record encapsulates all our programs runtime options.
data Settings = Settings
  { userId     :: Maybe Int -- ^ An optional target user ID
  , userSystem :: Bool -- ^ Is this a system user?
  , userGroups :: [Text] -- ^ Groups the new user will be in
  , userName   :: Text  -- ^ Username for the new user
  } deriving (Show)
\end{code}

Let's also pretend that our program's logic lives inside a function
`run :: Settings -> IO ()`. We pass it the settings we want, and it
runs the program accordingly. However, since this is just an example
program, we won't actually create any user accounts; instead we'll
just have the program print its settings to `stdout`.

\begin{code}
run :: Settings -> IO ()
run = print
\end{code}

Parser Types
------------

We want to construct a parser that reads a list of arguments and
yields a `Settings`. That means our parser will have the type
`UnixParser Settings`.

`UnixParser` is actually a type synonym for `ParseTree UnixScheme`,
where `UnixScheme` is a parsing scheme for UNIX-style command line
syntax. Alternative parsing schemes can be defined, but that's beyond
the scope of this example.

Now, let's examine the building blocks of parsers. We'll start with
the most basic components and work our way toward more complicated
parsers.

Text Parsers
------------

`TextParser`s are the most basic type of parser in Mangrove. Here's
the definition for reference:

< data TextParser r = TextParser
<   { parserHint :: !Text
<   , parserRun  :: Text -> Either Text r
<   }

We can see this is just a wrapper around a simple text parsing
function, with the addition of a "hint". The hint is just a short
descriptive string we can use to represent an input when displaying
usage information. It's traditional to use a single word in all-caps
for this, like `PATH`, `STRING`, or `INPUT_FILE`.

Mangrove provides text parsers for common types like `Int`, `Double`,
`Text`, and `String` in the `Mangrove.TextParser` module. However, we
can also let the compiler choose an appropriate text parser based on
the type. `defaultParser :: DefaultParser a => TextParser a` is a
polymorphic text parser that selects a reasonable parser
implementation for any type with a `DefaultParser` instance.

Positional Parameters
---------------------

A "positional parameter" is a positional input that accepts the first
non-flag argument it encounters. In Mangrove, positional parameters
are usually just referred to as "parameters" since other kinds of
parameters have their own names.

Consider an example program called `substring` whose command line
syntax is `substring START END STRING`. `START`, `END`, and `STRING`
would be parameters. If we invoke `substring 1 3 "example"`, we know
that `START` is `1`, `END` is `3`, and `STRING` is `"example"` because
of the order in which they appear.

Our program will have just one parameter: a username. Here is how we
define a parser for it:

\begin{code}
prm_name :: UnixParser Text
prm_name = parameter (defaultParser {parserHint = "USERNAME"})
\end{code}

The `parameter` function creates a parameter parser from a
`TextParser`. The `defaultParser` implementation for strict `Text` is
chosen based on the required types, and we choose to override the
default hint (normally `"STRING"`) with `"USERNAME"` which is more
descriptive.

Options
-------

An "option" represents a named input. Options consist of a flag
followed by an optional subargument string.

A "flag" is special symbol that signals the beginning of a particular
option. Per UNIX tradition there are long flags (e.g. `--foo`) and
short flags (e.g `-f`).

To prevent ambiguity, sometimes an equals sign is used to separate a
long flag from its subargument string (instead of a space). For
example, `--uid=1000` is an option that begins with the `--uid` flag
and is followed by the subargument string `1000`. Similarly, an
option's short flag can be directly concatenated with its argument,
e.g. `-u 1000` can be written `-u1000`.

Let's define a parser for the `--uid` option, which allows the user to
specify a user ID for the new user if they want. It should accept one
subparameter, an integer UID.

\begin{code}
opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"]
          "Specify a user ID"
          $ subparameter defaultParser
\end{code}

Switches
--------

The `--system` option is simpler because it doesn't accept any
subarguments - it is either present (`True`) or absent (`False`). This
special type of option is a "switch", and we can use the `switch`
function to create a parser:

\begin{code}
opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"
\end{code}

If we defined this without 'switch' it would look like this:

< opt_system = option ["--system", "-s"]
<              "Create a system user"
<              (pure True)
<              <|> pure False


Options with Multiple Subparameters
-----------------------------------

Let's deal with the `--groups` option. This option is a bit different
from the `--uid` option because we want the user to be able to specify
a list of groups for the new user to join. Thus, we want to create an
option that accepts one or more subarguments.

Thankfully, `SubParser` is also an `Alternative` instance. We can use
`some` (from `Control.Applicative`) to convert a `SubParser r` into a
`SubParser [r]` that will expect to parse one or more `r` values.

\begin{code}
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser {parserHint = "GROUP"}
\end{code}

Mangrove recognizes that the subparser `some $ subparameter
defaultParser :: SubParser [Text]` can consume multiple subarguments,
so it automatically splits the subarguments by comma. This allows us
to pass a list of group names like so: `--groups=wheel,audio,input`,
and the parser will yield `["wheel","audio","input"]`.

By using `some` instead of the similar function `many`, we have
created a subparser that will fail if no subarguments are provided
(e.g. `mkuser alice --groups`).

Applicative
-----------

You might notice that the `opt_uid` parser we constructed earlier has
the type `UnixParser Int`, but to eventually construct a `Settings`,
we really want a `Maybe Int`. That's because we haven't accounted for
the fact that the `--uid` might not be present in the arguments.
That's easy to fix using `optional` from `Control.Applicative`.

For the `--groups` option, we should simply get an empty list when the
option is missing.

Now we can finally construct our `Settings` parser:

\begin{code}
parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name
\end{code}

Request Options
---------------

The CLI interface we've defined is missing something important: an
option for displaying help and usage information. Let's create a new
`Settings` parser that recognizes `--help` as a request for help
information. While we're at it, we can also add a `--version` option
that displays the program's version.

To accomplish this without cluttering up our `Settings` structure, we
can use "request options". Request options are a special kind of
option which represent a query for information about the system. When
a request option is matched, the parser will abandon any further
parsing and instead provide a human-readable response to the query.

There are currently 2 kinds of requests: `HelpRequests` and
`VersionRequests`. A help request is a request for help and usage
information for our parser. A version request is for querying the
program's version.

\begin{code}
parseSettings' :: UnixParser Settings
parseSettings' = opt_help <|> opt_version <|> parseSettings
  where
    opt_help =
      requestOption ["--help"]
      "Display help and usage information"
      HelpRequest
    opt_version =
      requestOption ["--version"]
      "Display program version"
      VersionRequest
\end{code}

Running the Parser
------------------

In order to display help or version information, Mangrove needs a few
details about the program. This metadata is passed in using a
`ProgramInfo` record:

\begin{code}
-- Here the type variable @s@ is a phantom type that we can leave
-- undetermined.
programInfo :: ProgramInfo s
programInfo = ProgramInfo
  { programName = "mkuser" -- The name of the program
  , programVersion = makeVersion [0, 1, 2, 3] -- The program version is "0.1.2.3"
  , programDesc = "Create user accounts" -- A short description of the program
  }
\end{code}

The `parseArguments` function will run our parser with the arguments
passed to our program by the operating system.

\begin{code}
main :: IO ()
main = parseArguments programInfo parseSettings' run
\end{code}

`parseArguments` takes three arguments: the program metadata, a
parser, and a function of type `r -> IO a`. When the parser completes
successfully, this function will be called with the result. If parsing
fails, an error message will be printed to `stderr` and the program
will exit indicating failure. If a request for help or version
information is triggered, the program will respond by printing the
requested information to `stdout` and then exiting normally.

If you want to run an argument parser without using `IO`, or you want
to pass your own argument list, check out `runHelpfulParser` from
`Mangrove`.

Now we have a complete program we can build and run to show the
argument parser in action! Here's how the completed program behaves
with different inputs:

```
$ ./mkuser --system --groups audio,input bilbo
Settings {userId = Nothing, userSystem = True, userGroups = ["audio","input"], userName = "bilbo"}

$ ./mkuser --badinput
unexpected --badinput

$ ./mkuser --system
expected: USERNAME

$ ./mkuser --uid=InvalidNumber bilbo
--uid=InvalidNumber: InvalidNumber: input does not start with a digit
```
