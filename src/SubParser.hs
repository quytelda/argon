{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module SubParser where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TBL

import           Parser
import           ParseTree
import           StreamParser
import           Text

-- | Parsers for subarguments of an option, i.e. '--option key=value'.
data SubParser r
  = SubParameter (TextParser r)
  | SubAssoc Text (TextParser r)
  deriving (Functor)

instance HasValency SubParser where
  valency _ = Just 1

instance Resolve SubParser where
  resolve (SubParameter (TextParser hint _)) =
    throwError $ "expected " <> TBL.fromText hint
  resolve (SubAssoc key (TextParser hint _)) =
    throwError $ "expected " <> TBL.fromText key <> "=" <> TBL.fromText hint

instance Render (SubParser r) where
  render (SubParameter tp) = render $ parserHint tp
  render (SubAssoc key tp) = render key <> "=" <> render (parserHint tp)

instance Render (ParseTree SubParser r) where
  -- special cases
  render (SumNode p (ValueNode _)) = "[" <> render p <> "]"

  render EmptyNode                 = "EMPTY"
  render (ValueNode _)             = "VALUE"
  render (ParseNode parser)        = render parser
  render (MapNode _ p)             = render p
  render (ProdNode _ l r)          = render l <> "," <> render r
  render (SumNode l r)             = render l <> " | " <> render r
  render (ManyNode p)              = "[" <> render p <> "...]"

instance Parser SubParser where
  data Token SubParser
    = SubKeyValue Text Text -- ^ A key=value argument
    | SubArgument Text -- ^ A standard argument
    deriving (Show)

  parseTokens = fmap parse
    where
      parse (keyEqualsValue -> Just (k, v)) = SubKeyValue k v
      parse s                               = SubArgument s

  accepts (SubParameter _) (SubArgument _)   = True
  accepts (SubAssoc key _) (SubKeyValue k _) = key == k
  accepts _ _                                = False

  feedParser (SubParameter tp) = do
    peek >>= \case
      SubArgument s -> pop *> runTextParser tp s
      _             -> empty
  feedParser (SubAssoc key tp) = do
    peek >>= \case
      SubKeyValue k v | key == k -> pop *> runTextParser tp v
      _                          -> empty

instance Render (Token SubParser) where
  render (SubKeyValue k v) = render k <> "=" <> render v
  render (SubArgument s)   = render s
