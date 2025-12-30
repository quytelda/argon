{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Parser.Sub
  ( SubParser(..)
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           Parser
import           Parser.Text
import           Stream
import           Text

-- | Parsers for subarguments of an option, i.e. '--option key=value'.
data SubParser r
  = SubParameter (TextParser r)
  | SubOption Text (TextParser r)
  deriving (Functor)

instance HasValency SubParser where
  valency _ = Just 1

instance Resolve SubParser where
  resolve (SubParameter (TextParser hint _)) =
    throwError $ "expected " <> TLB.fromText hint
  resolve (SubOption key (TextParser hint _)) =
    throwError $ "expected " <> TLB.fromText key <> "=" <> TLB.fromText hint

instance Render (SubParser r) where
  render = renderParser

instance Parser SubParser where
  data Token SubParser
    = SubAssoc Text Text -- ^ A key=value argument
    | SubArgument Text -- ^ A standard argument
    deriving (Eq, Show)

  renderToken (SubAssoc k v)  = render k <> "=" <> render v
  renderToken (SubArgument s) = render s

  parseTokens = fmap parse
    where
      parse (keyEqualsValue -> Just (k, v)) = SubAssoc k v
      parse s                               = SubArgument s

  sepProd _ = ","
  sepSum _ = " | "

  renderParser (SubParameter tp)  = render $ parserHint tp
  renderParser (SubOption key tp) = render key <> "=" <> render (parserHint tp)

  accepts (SubParameter _) (SubArgument _) = True
  accepts (SubOption key _) (SubAssoc k _) = key == k
  accepts _ _                              = False

  feedParser (SubParameter tp) = do
    peek >>= \case
      SubArgument s ->
        withContext (render (parserHint tp) <> " subparameter") $
        pop_ *> runTextParser tp s
      _             -> empty
  feedParser (SubOption key tp) = do
    peek >>= \case
      SubAssoc k v
        | key == k ->
            withContext ("\"" <> render key <> "\" suboption") $
            pop_ *> runTextParser tp v
      _                          -> empty
