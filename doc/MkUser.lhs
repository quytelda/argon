Tutorial
========

Suppose we are writing a simple program that creates new user
accounts - we'll call it "mkuser". The goal will be to provide a
command line interface with the following syntax:

```
mkuser [--uid=INT] [--system] [--groups={GROUP...}]
  [--quota=[soft=INT],[hard=INT]] USERNAME
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

> {-# LANGUAGE OverloadedLists   #-}
> {-# LANGUAGE OverloadedStrings #-}

Now let's import the modules we need. The building blocks for our
parser are found in `Mangrove.Unix` while the functions for running
parsers live in the `Mangrove` module. We'll use combinators from
`Control.Applicative` to construct our parser. We'll also import
`Text`, since Mangrove is built around `Text` rather than `String`.
Finally, we'll use `Data.Version` when specifying program metadata.

> import           Control.Applicative
> import           Data.Text           (Text)
> import           Mangrove
> import           Mangrove.Unix
> import           Data.Version

Next let's create a new record that captures the program's runtime
configuration:

> -- | This record encapsulates all our programs runtime options.
> data Settings = Settings
>   { userId     :: Maybe Int -- ^ An optional target user ID
>   , userSystem :: Bool -- ^ Is this a system user?
>   , userGroups :: [Text] -- ^ Groups the new user will be in
>   , userQuota  :: Maybe Quota -- ^ Disk usage quotas
>   , userName   :: Text  -- ^ Username for the new user
>   } deriving (Show)

> -- | This record represents disk usage quota settings for a new user.
> data Quota = Quota
>   { quotaSoft :: Maybe Int -- ^ Limit new data after this point
>   , quotaHard :: Int -- ^ Hard upper limit on disk usage
>   } deriving (Show)

Let's also pretend that our program's logic lives inside a function
`run :: Settings -> IO ()`. We pass it the settings we want, and it
runs the program accordingly. However, since this is just an example
program, we won't actually create any user accounts; instead we'll
just have the program print its settings to `stdout`.

> run :: Settings -> IO ()
> run = print

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
`Text`, and `String` in the `Mangrove.TextParser` module.
Additionally, instead of manually specifying a parser for each type,
we can rely on Haskell's type system to choose one automatically. The
`defaultParser` function provides this capability: it's a polymorphic
parser for any type that has a `DefaultParser` instance (such as the
aforementioned common types). Thus, when `defaultParser` is used in a
context where the type known, Haskell automatically selects the
appropriate parser for that type.

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

Our `mkuser` program only needs one positional parameter: a username.
We define a parser for it using the `parameter` function, which
converts a `TextParser` into a positional parameter parser. Because a
`DefaultParser` instance exists for strict `Text`, we use
`defaultParser`; however, we override the parser's hint (normally
`"STRING"`) with `"USERNAME"` which is more descriptive.

> prm_name :: UnixParser Text
> prm_name = parameter (defaultParser {parserHint = "USERNAME"})

Options
-------

An "option" is a named input. It consists of a flag (like `--uid` or
`-u`) followed by an optional subargument string. Per UNIX tradition,
long flags use double dashes and short flags use a single dash. For
long flags, the subargument can be separated by an equal sign
(`--uid=1000`) or a space (`--uid 1000`). Short flags can be directly
concatenated with their subargument (`-u1000`) or separated by a space
(`-u 1000`).

Let's define a parser for the `--uid` option, which allows the user to
specify a user ID for the new user if they want. It should accept one
subparameter, an integer UID.

> opt_uid :: UnixParser Int
> opt_uid = option ["--uid", "-u"]
>           "Specify a user ID"
>           $ subparameter defaultParser

Switches
--------

The `--system` option is simpler because it doesn't accept any
subarguments - it is either present (`True`) or absent (`False`). This
special type of option is a "switch", and we can use the `switch`
function to create a parser:

> opt_system :: UnixParser Bool
> opt_system = switch ["--system", "-s"] "Create a system user"

If we defined this without 'switch' it would look like this:

< opt_system = option ["--system", "-s"]
<              "Create a system user"
<              (pure True)
<              <|> pure False


Options with Multiple Subparameters
-----------------------------------

Now that we've seen options with no subarguments, and options with a
single subargument, let's try defining an option that can accept
multiple subarguments.

The `--groups` option is a bit different from the `--uid` option
because we want the user to be able to specify a list of groups for
the new user to join. Thus, we want to create an option that accepts
one or more subarguments.

Thankfully, `SubParser` is also an `Alternative` instance. We can use
`some` (from `Control.Applicative`) to convert a `SubParser r` into a
`SubParser [r]` that will expect to parse one or more `r` values.

> opt_groups :: UnixParser [Text]
> opt_groups =
>   option ["--groups", "-g"]
>   "Specify what groups the user is part of"
>   $ some $ subparameter defaultParser {parserHint = "GROUP"}

Mangrove recognizes that the subparser `some $ subparameter
defaultParser :: SubParser [Text]` can consume multiple subarguments,
so it automatically splits the subarguments by comma. This allows us
to pass a list of group names like so: `--groups=wheel,audio,input`,
and the parser will yield `["wheel","audio","input"]`.

By using `some` instead of the similar function `many`, we have
created a subparser that will fail if no subarguments are provided
(e.g. `mkuser alice --groups`).

Suboptions
----------

So far, we've seen options that accept any number of subparameters.
But what if we need an option that accepts _structured_ data - not
just a list of values, but key/value pairs? This is common in
configuration and deployment tools. For example, a mount command might
need to specify both a source and destination path, along with
optional flags. Rather than forcing the user to remember exact
positional order, suboptions use named pairs: `--mount
src=/webroot,dst=/var/www,ro`.

A suboption is a named key/value pair within a single option's
arguments. Unlike positional subarguments (which are parsed in order),
suboptions are identified by name, so they can appear in any order.
Suboptions are separated by commas (just like positional
subarguments), but each suboption has the syntax `key=value`. You can
also mix suboptions with positional subarguments in the same option,
and Mangrove will parse all of them together.

When Mangrove begins parsing a list of subarguments for some option,
it checks whether the parse tree contains any suboption parsers. If it
does, Mangrove treats any subarguments that contain an `=` sign as
key/value pairs rather than positional subarguments. This mechanism
prevents confusion between the two parsing modes and ensures that
suboptions and subparsers don't interfere with each other. If the
parser subtree contains _no_ suboption parsers at all, Mangrove
ignores the `=` sign and treats all subarguments as positional.

Like regular options, suboptions can be optional or required. You can
use the same Applicative combinators to express this: `optional` makes
a suboption optional (returning `Nothing` if absent), and `<|>` allows
you to provide a default value.

Let's add a `--quota` option to our `mkuser` program for configuring a
user's disk usage quota. `--quota` will accept two suboptions: `soft`
and `hard`, for soft and hard space limits respectively. Each
suboption is itself optional and accepts a single integer value.

> opt_quota :: UnixParser Quota
> opt_quota =
>   option ["--quota"]
>   "Set a disk usage quota"
>   $ Quota <$> subopt_soft
>           <*> subopt_hard
>   where
>     subopt_soft :: SubParser (Maybe Int)
>     subopt_soft = optional $ suboption "soft" defaultParser

>     subopt_hard :: SubParser Int
>     subopt_hard = suboption "hard" defaultParser <|> pure 512

Applicative
-----------

You might notice that `opt_uid` has the type `UnixParser Int`, but our
`Settings` record expects `Maybe Int`. That's because we need to
handle the case where no `--uid` option appears in the arguments. The
solution is to wrap the parser using `optional` from
`Control.Applicative`: `optional opt_uid` attempts to parse the
`--uid` option, returning `Nothing` if it's absent.

For `--groups`, we want slightly different behavior: if the user
doesn't provide `--groups`, we should use an empty list instead of
returning `Nothing`. We can express this using the `<|>` operator
(i.e. the alternative operator): `opt_groups <|> pure []` means "try
parsing `--groups`, and if that fails, just return an empty list."

Now we can finally construct our `Settings` parser using Applicative
notation:

> parseSettings :: UnixParser Settings
> parseSettings =
>   Settings
>   <$> optional opt_uid
>   <*> opt_system
>   <*> (opt_groups <|> pure [])
>   <*> optional opt_quota
>   <*> prm_name

Request Options
---------------

The CLI interface we've defined is missing something important: an
option for displaying help and usage information. Let's create a new
`Settings` parser that recognizes `--help` as a request for help
information. While we're at it, we can also add a `--version` option
that displays the program's version.

Some options, like `--help` and `--version`, don't produce values that
go into our `Settings` record. Instead, they interrupt parsing to
provide information (help text or version number) and exit. Mangrove
calls these "request options". When a request option is matched,
parsing stops immediately and Mangrove prints the requested
information without trying to fill in the rest of `Settings`.

There are currently 2 kinds of requests: `HelpRequests` and
`VersionRequests`. A help request is a request for help and usage
information for our parser. A version request is for querying the
program's version.

> parseSettings' :: UnixParser Settings
> parseSettings' = opt_help <|> opt_version <|> parseSettings
>   where
>     opt_help =
>       requestOption ["--help"]
>       "Display help and usage information"
>       HelpRequest
>     opt_version =
>       requestOption ["--version"]
>       "Display program version"
>       VersionRequest

Running the Parser
------------------

In order to display help or version information, Mangrove needs a few
details about the program. This metadata is passed in using a
`ProgramInfo` record:

> -- Here the type variable @s@ is a phantom type that we can leave
> -- undetermined.
> programInfo :: ProgramInfo s
> programInfo = ProgramInfo
>   { programName = "mkuser" -- The name of the program
>   , programVersion = makeVersion [0,1,2,3] -- The program version is "0.1.2.3"
>   , programDesc = "Create user accounts" -- A short description of the program
>   }

The `parseArguments` function will run our parser with the arguments
passed to our program by the operating system.

> main :: IO ()
> main = parseArguments programInfo parseSettings' run

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
Settings {userId = Nothing, userSystem = True, userGroups =
["audio","input"], userQuota = Nothing, userName = "bilbo"}

$ ./mkuser --quota soft=500,hard=600 frodo
Settings {userId = Nothing, userSystem = False, userGroups = [],
userQuota = Just (Quota {quotaSoft = Just 500, quotaHard = 600}),
userName = "frodo"}

$ ./mkuser --badinput
unexpected --badinput

$ ./mkuser --system
expected: USERNAME

$ ./mkuser --uid=InvalidNumber bilbo
--uid=InvalidNumber: InvalidNumber: input does not start with a digit
```
