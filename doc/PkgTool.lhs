Building a Package Manager CLI
==============================

This tutorial assumes you are already familiar with the
basics of creating parsers for options and parameters. If not, you
might want to checkout the mkuser tutorial first.

Let's build a command-line interface for a simple package manager that
we'll call "pkgtool". This example demonstrates how to use Mangrove's
command system to create hierarchical CLIs where different commands
have different options and arguments.

`pkgtool` will support support four commands: `install`, `remove`,
`search`, and `list`, each with its own distinct set of options.
Here's a sketch of what the program's CLI syntax should look like:

```
pkgtool [--verbose] install [--version=STRING] PACKAGE
pkgtool [--verbose] remove [--force] PACKAGE
pkgtool [--verbose] search STRING
pkgtool [--verbose] list [--all]
```

This tutorial exists as a literate Haskell program in the `doc`
directory of the project's repository. That means it is also a valid
Haskell source file that you can compile:

```
$ stack exec ghc -- -o pkgtool doc/PkgTool.lhs
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
> import           Data.Version
> import           Mangrove
> import           Mangrove.Unix

Next let's create a new record that captures the program's runtime
configuration:

Data Types
----------

To represent the different commands our tool supports, we'll create
two data structures. The `Settings` record captures global options
(like `--verbose`), while the `Mode` sum type represents each distinct
subcommand with its own set of settings. We use a sum type because
each command represents a mutually exclusive runtime mode.

> data Settings = Settings
>   { sVerbose :: Bool -- ^ Enable debugging output
>   , sMode    :: Mode -- ^ What runtime mode is being invoked?
>   } deriving (Show)
>
> -- | This type alias makes our data structure more readable.
> type PackageName = Text
>
> data Mode
>   = InstallMode (Maybe Text) PackageName
>   | RemoveMode Bool PackageName
>   | SearchMode Text
>   | ListMode Bool
>   deriving (Show)

Each constructor of `Mode` holds the specific data for that command:

- `InstallMode` captures an optional version string and a required
  package name
- `RemoveMode` captures a boolean switch (for `--force`) and a package
  name
- `SearchMode` captures a search query string
- `ListMode` captures a boolean for whether to show all packages

Defining Commands
-----------------

The simplest way to define a command is to use the `command` function,
which we supply with a list of command names, a description for help
output, and a parser for all the options, parameters, and subcommands
the command expects.

> -- | A parameter that reads a package name.
> prm_package :: UnixParser PackageName
> prm_package = parameter defaultParser { parserHint = "PACKAGE" }
>
> cmd_install :: UnixParser Mode
> cmd_install =
>   command ["install"]
>   "Install a package"
>   $ InstallMode <$> optional opt_version
>                 <*> prm_package
>   where
>     opt_version =
>       option ["--version"]
>       "Package version to install"
>       $ subparameter defaultParser

The `install` command accepts a `--version` option followed by a
package name parameter. `--version` expects a single subargument
representing a target version number (which just store as `Text` in
this example). Since `--version` isn't a required option, we wrap it
with `optional`.

> cmd_remove :: UnixParser Mode
> cmd_remove =
>   command ["remove"]
>   "Remove a package"
>   $ RemoveMode <$> opt_force
>                <*> prm_package
>   where
>     opt_force =
>       switch ["--force"]
>       "Remove package without safety checks"

The `remove` command is simpler: it accepts and optional `--force`
option (a switch) and then a required package name.

> cmd_search :: UnixParser Mode
> cmd_search =
>   command ["search"]
>   "Search for matching packages"
>   $ SearchMode <$> parameter defaultParser

The `search` command is the simplest in the set; it takes a single
text text parameter (the search query) and no options.

> cmd_list :: UnixParser Mode
> cmd_list =
>   command ["list", "ls"]
>   "List packages"
>   $ ListMode <$> opt_all
>   where
>     opt_all =
>       switch ["--all"]
>       "Include packages not currently installed"

The `list` command supports an optional `--all` option. Notice that we
also give the command an alias: `["list", "ls"]`. This means users can
type either `pkgtool list` or `pkgtool ls` to invoke the same command.

Combining Commands into a Parser
--------------------------------

So far, we've defined each command independently. Now we need to tell
Mangrove that the user will choose _one_ of these commands. We do this
using the `<|>` operator (i.e. the "alternative" combinator), which
tries each parser in sequence until one succeeds.

> parseMode :: UnixParser Mode
> parseMode =
>   cmd_install
>   <|> cmd_remove
>   <|> cmd_search
>   <|> cmd_list

Putting it all Together
-----------------------

At the top level, we want to support the `--verbose` flag, which can
be used with any command. This global option is separate from the
subcommands.

We can now construct a `Settings` parser via the usual applicative
methods. We use `addHelpOptions` to add support for a `--help` option.
Instead of just adding a single global `--help` option, the
`addHelpOptions` function adds a `--help` option at the root of each
command tree. That means we can invoke `pkgtool install --help` to get
help information about the `install` command specifically.

> parseSettings :: UnixParser Settings
> parseSettings =
>   addHelpOptions ["--help", "-h"]
>   "Display help and usage information"
>   $ Settings <$> opt_verbose
>              <*> parseMode
>   where
>     opt_verbose =
>       switch ["--verbose", "-v"]
>       "Log debugging messages"
> 
> main :: IO ()
> main = parseArguments programInfo parseSettings print
>   where
>     programInfo = ProgramInfo
>       { programName = "pkgtool"
>       , programVersion = makeVersion [1,3]
>       , programDesc = "A simple package manager"
>       }

Here are some examples use of `pkgtool`:

```
$ ./pkgtool install curl
Settings {sVerbose = False, sMode = InstallMode Nothing "curl"}

$ ./pkgtool --verbose install --version 2.1.0 curl
Settings {sVerbose = True, sMode = InstallMode (Just "2.1.0") "curl"}

$ ./pkgtool remove --force vim
Settings {sVerbose = False, sMode = RemoveMode True "vim"}

$ ./pkgtool search "web server"
Settings {sVerbose = False, sMode = SearchMode "web server"}

$ ./pkgtool ls --all
Settings {sVerbose = False, sMode = ListMode True}
```

And here's what happens if the user provides invalid input:

```
$ ./pkgtool --verbose
expected: install or remove or search or list

$ ./pkgtool remove --version=2.1.0 curl
remove: expected: --help or PACKAGE
```
