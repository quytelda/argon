{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Stream where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.List                 as List

-- | A stream parameterized by token type.
type Stream tok = StateT [tok] (Except String)

runStream :: Stream tok a -> [tok] -> Either String (a, [tok])
runStream m = runExcept . runStateT m

isEmptyStream :: Stream tok Bool
isEmptyStream = gets null

pop :: Stream tok (Maybe tok)
pop = state $ \case
  [] -> (Nothing, [])
  (x:xs) -> (Just x, xs)

peek :: Stream tok (Maybe tok)
peek = gets $ fmap fst . List.uncons

push :: tok -> Stream tok ()
push s = modify' (s:)
