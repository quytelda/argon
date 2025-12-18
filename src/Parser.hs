{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Parser where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Functor
import           Data.Kind
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Stream

-- | Valency represents the maxiumum number of arguments a parser
-- might consume.
--
-- If the valency of a parser is 'Just n', then it consumes up to 'n'
-- arguments. If the valency is 'Nothing', it can consume an arbitrary
-- number of arguments.
class HasValency p where
  valency :: p r -> Maybe Integer

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: f r -> Except String r

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class Resolve p => Parser (p :: Type -> Type) where
  data Token p
  parseTokens :: [Text] -> [Token p]
  renderTokens :: [Token p] -> [Text]

  accepts :: p r -> Token p -> Bool
  feedParser :: p r -> MaybeT (Stream (Token p)) r
