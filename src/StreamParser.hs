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
import qualified Data.Text.Lazy.Builder as TLB

type Context = Builder

data ParseResult tok a
  = ParseResult [Context] [tok] a
  | ParseEmpty [Context] [tok]
  | ParseError [Context] Builder
  deriving (Functor)

instance Semigroup (ParseResult tok a) where
  l <> r =
    case (l, r) of
      (ParseError _ _, _)    -> l
      (_, ParseError _ _)    -> r
      (ParseEmpty _ _, _)    -> r
      (ParseResult _ _ _, _) -> l

newtype StreamParser tok a = StreamParser
  { runStreamParser :: [Context] -> [tok] -> ParseResult tok a }
  deriving (Functor)

instance Applicative (StreamParser tok) where
  pure a = StreamParser $ \cs ts -> ParseResult cs ts a
  mf <*> ma = StreamParser $ \cs ts ->
    case runStreamParser mf cs ts of
      ParseResult cs' ts' f -> f <$> runStreamParser ma cs' ts'
      ParseEmpty cs' ts'    -> ParseEmpty cs' ts'
      ParseError cs' err    -> ParseError cs' err

instance Alternative (StreamParser tok) where
  empty = StreamParser $ \s -> ParseEmpty s
  l <|> r = StreamParser $ \s ->
    runStreamParser l s <> runStreamParser r s

instance Monad (StreamParser tok) where
  return = pure
  mf >>= f = StreamParser $ \cs ts ->
    case runStreamParser mf cs ts of
      ParseResult cs' ts' a -> runStreamParser (f a) cs' ts'
      ParseEmpty cs' ts'    -> ParseEmpty cs' ts'
      ParseError cs' e      -> ParseError cs' e

instance MonadError Builder (StreamParser tok) where
  throwError err = StreamParser $ \cs _ -> ParseError cs err
  catchError ma handler = StreamParser $ \cs ts ->
    case runStreamParser ma cs ts of
      ParseError cs' err -> runStreamParser (handler err) cs' ts
      result             -> result

--------------------------------------------------------------------------------

getContexts :: StreamParser tok [Context]
getContexts = StreamParser $ \cs ts -> ParseResult cs ts cs

pushContext :: Context -> StreamParser tok ()
pushContext context = StreamParser $ \cs ts -> ParseResult (context : cs) ts ()

popContext :: StreamParser tok (Maybe Context)
popContext = StreamParser $ \cs ts ->
  case cs of
    (c : cs') -> ParseResult cs' ts $ Just c
    _         -> ParseResult cs ts Nothing

withContext :: Context -> StreamParser tok a -> StreamParser tok a
withContext context action = pushContext context *> action <* popContext

--------------------------------------------------------------------------------

popMaybe :: StreamParser tok (Maybe tok)
popMaybe = StreamParser $ \cs ts ->
  case ts of
    (t:ts') -> ParseResult cs ts' $ Just t
    _       -> ParseResult cs ts Nothing

peekMaybe :: StreamParser tok (Maybe tok)
peekMaybe = StreamParser $ \cs ts ->
  case ts of
    (t:_) -> ParseResult cs ts $ Just t
    _     -> ParseResult cs ts Nothing

pop :: StreamParser tok tok
pop = StreamParser $ \cs ts ->
  case ts of
    (t:ts') -> ParseResult cs ts' t
    _       -> ParseEmpty cs ts

peek :: StreamParser tok tok
peek = StreamParser $ \cs ts ->
  case ts of
    (t:_) -> ParseResult cs ts t
    _     -> ParseEmpty cs ts

push :: tok -> StreamParser tok ()
push t = StreamParser $ \cs ts -> ParseResult cs (t:ts) ()

--------------------------------------------------------------------------------

liftExcept :: Except Builder a -> StreamParser tok a
liftExcept m = StreamParser $ \cs ts ->
  case runExcept m of
    Left err -> ParseError cs err
    Right a  -> ParseResult cs ts a
