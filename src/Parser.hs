{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser
  ( HasValency(..)
  , valencyIs
  , ResolveError(..)
  , sumResults
  , Resolve(..)
  , resolveLifted
  , Parser(..)
  ) where

import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Kind
import qualified Data.List            as List
import           Data.Proxy
import           Data.Text            (Text)

import           Stream
import           Text

-- | Resolving an object might fail if not enough input has been
-- provided or the result depends on an unresolvable value (e.g.
-- empty).
data ResolveError
  = EmptyError
  | ExpectedError [Builder]
  deriving (Eq, Show)

instance Semigroup ResolveError where
  ExpectedError ls <> ExpectedError rs = ExpectedError $ ls <> rs
  l <> EmptyError                      = l
  EmptyError <> r                      = r

instance Render ResolveError where
  render EmptyError = "empty"
  render (ExpectedError ts) = "expected: " <> mconcat (List.intersperse " or " ts)

sumResults :: Either ResolveError r -> Either ResolveError r -> Either ResolveError r
sumResults (Left e1) (Left e2) = Left $ e1 <> e2
sumResults l r                 = l <> r

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
  resolve :: f r -> Either ResolveError r

resolveLifted :: Resolve f => f r -> StreamParser tok r
resolveLifted = liftEither . first render . resolve

-- | A type class for meant to parameterize 'ParseTree's. A parser can
-- consume input token and produce a result or throw an error.
class (Functor p, Resolve p) => Parser (p :: Type -> Type) where
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
