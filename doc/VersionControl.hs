{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Text           (Text)
import           Data.Version
import           Mangrove
import           Mangrove.Unix

--------------------------------------------------------------------------------
-- Committing

data CommitSettings = CommitSettings
  { commitAuthor  :: Text
  , commitMessage :: Text
  } deriving (Show)

parseCommitSettings :: UnixParser CommitSettings
parseCommitSettings =
  CommitSettings
  <$> prm_author
  <*> prm_message
  where
    prm_author  = parameter defaultParser {parserHint = "AUTHOR"}
    prm_message = parameter defaultParser {parserHint = "MESSAGE"}

data PullSettings = PullSettings
  { pullBranch :: Text
  , pullUrl    :: Text
  } deriving (Show)

parsePullSettings :: UnixParser PullSettings
parsePullSettings =
  PullSettings
  <$> opt_branch
  <*> prm_url
  where
    opt_branch =
      option ["--branch", "-b"]
      "Pull a specific branch by name"
      (subparameter defaultParser {parserHint = "BRANCH"})
      <|> pure "main"
    prm_url = parameter defaultParser {parserHint = "URL"}

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

-- | This record encapsulates all our programs runtime options.
data Settings = Settings
  { settingVerbose :: Bool
  , settingMode    :: Mode
  } deriving (Show)

parseSettings :: UnixParser Settings
parseSettings =
  Settings
  <$> opt_verbose
  <*> parseMode
  where
    opt_verbose =
      switch ["--verbose", "-v"]
      "Show more detailed output"

-- | We are pretending this function contains the primary logic of our
-- program. Since this is an example for illustrating the argument
-- parser, we simply print the parsed settings and exit.
run :: Settings -> IO ()
run = print

programInfo :: ProgramInfo s
programInfo = ProgramInfo
  { programName = "myvcs"
  , programVersion = makeVersion [0, 1, 2, 3]
  , programDesc = "Track and synchronize version history"
  }

main :: IO ()
main =
  parseArguments
  programInfo
  (addHelpOptions ["--help"] "Display help information" parseSettings)
  run
