{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module StreamParser where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TBL

data ParseResult tok a
  = ParseResult [tok] a
  | ParseEmpty [tok]
  | ParseError Builder
  deriving (Functor)

instance Semigroup (ParseResult tok a) where
  l <> r =
    case (l, r) of
      (ParseError _, _)    -> l
      (_, ParseError _)    -> r
      (ParseEmpty _, _)    -> r
      (ParseResult _ _, _) -> l

newtype StreamParser tok a = StreamParser { runStreamParser :: [tok] -> ParseResult tok a }
  deriving (Functor)

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \s -> ParseResult s a
  mf <*> ma = StreamParser $ \s1 ->
    case runStreamParser mf s1 of
      ParseResult s2 f -> f <$> runStreamParser ma s2
      ParseEmpty s2    -> ParseEmpty s2
      ParseError e     -> ParseError e

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \s -> ParseEmpty s
  l <|> r = StreamParser $ \s ->
    runStreamParser l s <> runStreamParser r s

instance Monad (StreamParser tok) where
  return = pure
  mf >>= f = StreamParser $ \s ->
    case runStreamParser mf s of
      ParseResult s' a -> runStreamParser (f a) s'
      ParseEmpty s'    -> ParseEmpty s'
      ParseError e     -> ParseError e

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \_ -> ParseError err
  catchError ma handler = StreamParser $ \s ->
    case runStreamParser ma s of
      ParseError err -> runStreamParser (handler err) s
      result         -> result

--------------------------------------------------------------------------------

popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \ts ->
  case ts of
    (t:ts') -> ParseResult ts' $ Just t
    _       -> ParseResult ts Nothing

peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \ts ->
  case ts of
    (t:_) -> ParseResult ts $ Just t
    _     -> ParseResult ts Nothing

pop :: StreamParser tok tok
pop = StreamParser $ \ts ->
  case ts of
    (t:ts') -> ParseResult ts' t
    _       -> ParseEmpty ts

peek :: StreamParser tok tok
peek = StreamParser $ \ts ->
  case ts of
    (t:_) -> ParseResult ts t
    _     -> ParseEmpty ts

push :: tok -> StreamParser tok ()
push t = StreamParser $ \ts -> ParseResult (t:ts) ()

--------------------------------------------------------------------------------

liftExcept :: Except Builder a -> StreamParser tok a
liftExcept m = StreamParser $ \ts ->
  case runExcept m of
    Left e  -> ParseError e
    Right a -> ParseResult ts a
