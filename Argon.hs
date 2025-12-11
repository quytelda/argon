{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Bifunctor
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.List.NonEmpty        as NonEmpty
import           Data.Text                 (Text)
import qualified Data.Text                 as T

-- | 'ParseTree p r' is an expression tree built from parsers of type
-- 'p' which evaluates to a value of type 'r' supplied with the proper
-- input.
data ParseTree p r where
  EmptyNode :: ParseTree p r -- ^ Terminal node with no value
  ValueNode :: r -> ParseTree p r -- ^ Terminal node with a resolved value
  ParseNode :: p r -> ParseTree p r -- ^ A parser awaiting input
  MapNode :: (a -> r) -> ParseTree p a -> ParseTree p r -- ^ Abstracts fmap
  ProdNode :: (u -> v -> r) -> ParseTree p u -> ParseTree p v -> ParseTree p r -- ^ Abstracts liftA2
  SumNode :: ParseTree p r -> ParseTree p r -> ParseTree p r -- ^ Abstracts (<|>)
  ManyNode :: ParseTree p r -> ParseTree p [r] -- ^ Abstracts many

instance Functor (ParseTree p) where
  fmap f = MapNode f

instance Applicative (ParseTree p) where
  pure = ValueNode
  liftA2 = ProdNode

instance Alternative (ParseTree p) where
  empty = EmptyNode
  (<|>) = SumNode
  many = ManyNode

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: f r -> Either String r

instance Resolve p => Resolve (ParseTree p) where
  resolve EmptyNode          = throwError "empty"
  resolve (ValueNode value)  = pure value
  resolve (ParseNode parser) = resolve parser
  resolve (MapNode f p)      = fmap f $ resolve p
  resolve (ProdNode f l r)   = f <$> resolve l <*> resolve r
  resolve (SumNode l r)      = resolve l <> resolve r
  resolve (ManyNode p)       = pure []

--------------------------------------------------------------------------------
-- Valency

-- | 'Arity' represents the number of input a parser-like object might
-- consume. We only care about three classes: 'Nullary' parsers
-- consume no input, 'Unary' parsers consume up to one input, and
-- 'Multary' parsers can potentially consume two or more inputs.
data Arity = Nullary | Unary | Multary
  deriving (Eq, Show, Ord)

instance Semigroup Arity where
  Nullary <> Nullary = Nullary
  Nullary <> Unary   = Unary
  Unary   <> Nullary = Unary
  _       <> _       = Multary

-- | Parser-like objects that can be analyzed to determine
-- valency/arity.
class Valency p where
  valency :: p r -> Arity

instance Valency p => Valency (ParseTree p) where
  valency EmptyNode        = Nullary
  valency (ValueNode _)    = Nullary
  valency (ParseNode _)    = Unary
  valency (MapNode _ p)    = valency p
  valency (ProdNode _ l r) = valency l <> valency r
  valency (SumNode l r)    = max (valency l) (valency r)
  valency (ManyNode _)     = Multary

--------------------------------------------------------------------------------
-- Stream Monad

type Stream = StateT [Text] (Except String)

runStream :: Stream a -> [Text] -> Either String (a, [Text])
runStream m = runExcept . runStateT m

isEmptyStream :: Stream Bool
isEmptyStream = gets null

pop :: Stream (Maybe Text)
pop = state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

peek :: Stream (Maybe Text)
peek = gets $ fmap fst . List.uncons

push :: Text -> Stream ()
push s = modify' (s:)

--------------------------------------------------------------------------------

class Accepts a where
  accepts :: a -> Text -> Bool

data Flag
  = LongOption Text
  | ShortOption Char
  deriving (Eq, Show)

type Flags = NonEmpty Flag

class ToFlag a where
  flag :: a -> Flags

instance ToFlag Char where
  flag = NonEmpty.singleton . ShortOption

instance ToFlag Text where
  flag = NonEmpty.singleton . LongOption

data OptionInfo = OptionInfo
  { optFlags :: Flags
  , optHelp  :: Text
  } deriving (Show)

instance Accepts OptionInfo where
  accepts OptionInfo{..} (T.stripPrefix "--" -> Just s) =
    LongOption s `elem` optFlags
  accepts OptionInfo{..} (T.stripPrefix "-" >=> T.uncons -> Just (c, "")) =
    ShortOption c `elem` optFlags
  accepts _ _ = False

optHead :: OptionInfo -> Flag
optHead info = NonEmpty.head $ optFlags info

type Commands = NonEmpty Text

command :: Text -> Commands
command = NonEmpty.singleton

data CommandInfo = CommandInfo
  { cmdNames :: Commands
  , cmdHelp  :: Text
  } deriving (Show)

instance Accepts CommandInfo where
  accepts CommandInfo{..} s = s `elem` cmdNames

--------------------------------------------------------------------------------

-- | The result of attempting to activate a parser
data ParserResult p r
  = Empty -- ^ The parser consumed no input (i.e. does not apply)
  | Partial (p r) -- ^ The parser consumed input and requires more
  | Done r -- ^ The parser consumed input and completed
  deriving (Functor)

mapParser :: (p r -> q r) -> ParserResult p r -> ParserResult q r
mapParser f Empty            = Empty
mapParser f (Done r)         = Done r
mapParser f (Partial parser) = Partial $ f parser

eitherToResult :: MonadError String m => Either String r -> m (ParserResult p r)
eitherToResult = either throwError (pure . Done)

-- | A type class for anything that can be a parsing node in a
-- 'ParseTree'.
class Parser p where
  feedParser :: p r -> Stream (ParserResult p r)

-- | A parser that accepts a simple text argument.
data TextParser r = TextParser Text (Text -> Either String r)
  deriving (Functor)

instance Valency TextParser where
  valency _ = Unary

instance Parser TextParser where
  feedParser (TextParser hint parse) = pop >>= \case
    Just s -> eitherToResult $ parse s
    Nothing -> throwError $ "expected " <> T.unpack hint <> ", got end-of-input"

instance Resolve TextParser where
  resolve (TextParser hint _) =
    throwError $ "TextParser: expected " <> T.unpack hint

-- | Parsers for the argument of an option, i.e. '--option key=value'.
data OptParser r
  = OptParameter (TextParser r) -- ^ A standard parameter
  | OptKey Text (TextParser r) -- ^ A key[=value] parameter
  deriving (Functor)

instance Valency OptParser where
  valency (OptParameter p) = valency p
  valency (OptKey _ p)     = valency p

instance Resolve OptParser where
  resolve (OptParameter parser) = first ("OptParameter: " <>) $ resolve parser
  resolve (OptKey key parser) =
    first (\s -> "OptKey (" <> T.unpack key <> "): " <> s) $ resolve parser

-- | Parse a 'Text' of the form "key=value" into ("key", "value"). If
-- the delimiter ('=') does not appear in the string, the result is
-- 'Nothing'.
breakKeyValue :: Text -> Maybe (Text, Text)
breakKeyValue s =
  case T.break (== '=') s of
    (key, T.uncons -> Just (_, value)) -> Just (key, value)
    _                                  -> Nothing

instance Parser OptParser where
  feedParser (OptParameter parser) = mapParser OptParameter <$> feedParser parser
  feedParser (OptKey key (TextParser _ parse)) = peek >>= \case
    Just s -> case breakKeyValue s of
      Just (k, v) | key == k ->
                    pop *> eitherToResult (parse v)
    _ -> pure Empty

-- | Parsers for top-level CLI arguments such as commands and options.
data CliParser r
  = CliParameter (TextParser r) -- ^ A simple parameter
  | CliOption OptionInfo (ParseTree OptParser r) -- ^ An option (e.g. '--option')
  | CliCommand Text (ParseTree CliParser r) -- ^ A subcommand
  deriving (Functor)

instance Valency CliParser where
  valency (CliParameter p)    = valency p
  valency (CliOption _ tree)  = Unary <> valency tree
  valency (CliCommand _ tree) = Unary <> valency tree

instance Parser CliParser where
  feedParser (CliParameter parser) = mapParser CliParameter <$> feedParser parser
  feedParser (CliOption info parser) = peek >>= \case
    Just s | info `accepts` s -> do
          -- consume option flag
          void pop

          -- parse the parameter, if any
          eitherToResult . resolve
            =<< case valency parser of
                  Multary -> pop >>= \case
                    Nothing -> throwError
                      $ "CliOption ("
                      <> show (optHead info)
                      <> "): missing argument"
                    Just s ->
                      case runStream (consume parser) (T.split (== ',') s) of
                        Left err       -> throwError err
                        -- TODO: handle leftover args
                        Right (res, _) -> pure res
                  _ -> consume parser
    _ -> pure Empty
  feedParser (CliCommand cmd tree) = peek >>= \case
    Just s | cmd == s ->
             pop -- consume command
             *> consume tree
             >>= eitherToResult . resolve
    _ -> pure Empty

instance Resolve CliParser where
  resolve (CliParameter parser) = first ("CliParameter: " <>) $ resolve parser
  resolve (CliOption info _)    = throwError $ "CliOption (" <> show (optHead info) <> ")"
  resolve (CliCommand cmd _)    = throwError $ "CliCommand (" <> T.unpack cmd <> ")"

--------------------------------------------------------------------------------
-- Feeding the Tree

-- | 'feed' traverses the tree until it activates a parser that
-- consumes input. Once a subtree consumes input, it is replaced with
-- an updated subtree and further traversal ceases.
feed :: Parser p => ParseTree p r -> MaybeT Stream (ParseTree p r)
feed EmptyNode = empty
feed (ValueNode _) = empty
feed (ParseNode parser) = lift (feedParser parser) >>= \case
  Done value      -> pure $ ValueNode value
  Partial parser' -> pure $ ParseNode parser'
  Empty -> empty
feed (MapNode f tree) = MapNode f <$> feed tree
feed (ProdNode f l r) =
  (ProdNode f <$> feed l <*> pure r) <|>
  (ProdNode f l <$> feed r)
feed (SumNode l r) =
  (SumNode <$> feed l <*> pure r) <|>
  (SumNode l <$> feed r)
feed (ManyNode tree) =
  ProdNode (:)
  <$> feed tree
  <*> pure (ManyNode tree)

-- | Repeatedly feed input to the tree using `feed` until either no
-- input is consumed after traversal (e.g. `feed` returns Nothing), or
-- no input remains.
consume :: Parser p => ParseTree p r -> Stream (ParseTree p r)
consume tree = do
  result <- runMaybeT $ feed tree
  case result of
    Just tree' -> do
      isEmpty <- isEmptyStream
      if isEmpty
        then pure tree'
        else consume tree'
    _ -> pure tree
