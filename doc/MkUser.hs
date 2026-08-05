{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Text           (Text)
import           Mangrove
import           Mangrove.Unix

-- | This record encapsulates all our programs runtime options.
data Settings = Settings
  { userId     :: Maybe Int -- ^ An optional target user ID
  , userSystem :: Bool -- ^ Is this a system user?
  , userGroups :: [Text] -- ^ Groups the new user will be in
  , userName   :: Text  -- ^ Username for the new user
  } deriving (Show)

-- | We are pretending this function contains the primary logic of our
-- program. Since this is an example for illustrating the argument
-- parser, we simply print the parsed settings and exit.
run :: Settings -> IO ()
run = print

-- | Here we define an option for specifying a user id for the new
-- user. It is triggered by the flags "--uid" or "-u" and requires one
-- integer parameter.
opt_uid :: UnixParser Int
opt_uid = option ["--uid", "-u"]
          "Specify a user ID"
          $ subparameter defaultParser

-- | Here we define a switch for indicating that the new user should
-- be a system user. If the "--system" flag or the "-s" flag are
-- present in the arguments, the parser will yield 'True'; otherwise
-- it will yield 'False'.
opt_system :: UnixParser Bool
opt_system = switch ["--system", "-s"] "Create a system user"

-- | This option specifies which groups the new user should be added
-- to. It requires a minimum of one group name as a subargument, but
-- can accept multiple group names as a comma separated list.
opt_groups :: UnixParser [Text]
opt_groups =
  option ["--groups", "-g"]
  "Specify what groups the user is part of"
  $ some $ subparameter defaultParser {parserHint = "GROUP"}

-- | This parameter expects a username. It will consume the first
-- argument that is not interpreted as an option.
prm_name :: UnixParser Text
prm_name = parameter defaultParser {parserHint = "USERNAME"}

-- | This is the combined argument parser that we construct from our
-- individual option and parameter parsers. We use 'optional' to wrap
-- the result of opt_uid in a 'Maybe' in case the option isn't
-- present. Similarly, we add the fallback case `pure []` after
-- 'opt_groups' for when the option is absent.
parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> optional opt_uid
  <*> opt_system
  <*> (opt_groups <|> pure [])
  <*> prm_name

-- | Here we add a `--help` option to our parser that will trigger
-- help output.
parseSettings' :: UnixParser Settings
parseSettings' = addHelpOptions ["--help"]
                 "Display help and usage information"
                 parseSettings

-- | Here we define some metadata for our program. This information
-- will be used for displaying help output and usage information if
-- necessary.
programInfo :: ProgramInfo
programInfo = ProgramInfo
  { programName = "mkuser" -- The name of the program
  , programDesc = "Create user accounts" -- A short description of the program
  }

main :: IO ()
main = do
  parseArguments
    programInfo
    parseSettings'
    run
