{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser
  ( HasValency(..)
  , valencyIs
  , Resolve(..)
  , Parser(..)
  ) where

import           Data.Kind
import           Data.Proxy
import           Data.Text              (Text)

import           Stream
import           Text

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
  resolve :: f r -> Either Builder r

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class Resolve p => Parser (p :: Type -> Type) where
  -- | The token type this parser operates upon.
  data Token p

  -- | A 'Parser' instance must provide a rendering function so that
  -- tokens can be displayed in error messages.
  renderToken :: Token p -> Builder

  -- | A 'Parser' instance needs to provide a function to parse its
  -- tokens from 'Text' inputs. We parse from '[Text]' to '[Token p]'
  -- since it is possible that a single 'Text' might yield multiple
  -- tokens (or none).
  parseTokens :: [Text] -> [Token p]

  sepProd :: Proxy p -> Builder
  sepSum :: Proxy p -> Builder

  -- | Generate usage information for a 'Parser' instance.
  renderParser :: p r -> Builder

  -- | Lift this parser into an appropriate 'StreamParser'.
  feedParser :: p r -> StreamParser (Token p) r

instance Parser p => Render (Token p) where
  render = renderToken
