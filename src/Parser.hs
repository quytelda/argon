{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Kind
import           Data.Text              (Text)
import qualified Data.Text.Lazy.Builder as TLB

import           StreamParser

-- | Valency represents the maxiumum number of arguments a parser
-- might consume.
--
-- If the valency of a parser is 'Just n', then it consumes up to 'n'
-- arguments. If the valency is 'Nothing', it can consume an arbitrary
-- number of arguments.
class HasValency p where
  valency :: p r -> Maybe Integer

-- | Utility function to check how many arguments something supports.
valencyIs :: HasValency p => (Integer -> Bool) -> p r -> Bool
valencyIs condition = all condition . valency

-- | Things that can be resolved to a value, but might fail to
-- resolve.
class Resolve f where
  resolve :: (Alternative m, MonadError TLB.Builder m) => f r -> m r

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class Resolve p => Parser (p :: Type -> Type) where
  data Token p
  parseTokens :: [Text] -> [Token p]

  accepts :: p r -> Token p -> Bool
  feedParser :: p r -> StreamParser (Token p) r
