{-# LANGUAGE GADTs #-}

module StructureEq
  ( StructureEq(..)
  ) where

import           Mangrove
import           Mangrove.ParseTree
import qualified Mangrove.Scheme.Sub  as Sub
import           Mangrove.Scheme.Unix
import qualified Mangrove.Scheme.Unix as Unix
import           Mangrove.TextParser
import           Mangrove.Unix

-- | Things that can be compared for structural equality.
class StructureEq s where
  structEq :: s a -> s b -> Bool

instance StructureEq TextParser where
  structEq tp1 tp2 = parserHint tp1 == parserHint tp2

instance StructureEq SubScheme where
  structEq (Sub.Parameter p1) (Sub.Parameter p2) =
    structEq p1 p2
  structEq (Sub.Option key1 p1) (Sub.Option key2 p2) =
    key1 == key2 && structEq p1 p2
  structEq _ _ =
    False

instance StructureEq UnixScheme where
  structEq (Unix.Parameter p1) (Unix.Parameter p2) =
    structEq p1 p2
  structEq (Unix.Option info1 subtree1) (Unix.Option info2 subtree2) =
    info1 == info2 && structEq subtree1 subtree2
  structEq (Unix.Command info1 subtree1) (Unix.Command info2 subtree2) =
    info1 == info2 && structEq subtree1 subtree2
  structEq (Unix.RequestOption info1 type1) (Unix.RequestOption info2 type2) =
    info1 == info2 && type1 == type2
  structEq _ _ =
    False

instance StructureEq s => StructureEq (ParseTree s) where
  structEq EmptyNode EmptyNode =
    True
  structEq (ValueNode _) (ValueNode _) =
    True
  structEq (ParseNode p1) (ParseNode p2) =
    structEq p1 p2
  structEq (ProdNode _ l1 r1) (ProdNode _ l2 r2) =
    structEq l1 l2 && structEq r1 r2
  structEq (SumNode l1 r1) (SumNode l2 r2) =
    structEq l1 l2 && structEq r1 r2
  structEq (ManyNode b1 p1) (ManyNode b2 p2) =
    b1 == b2 && structEq p1 p2
  structEq _ _ =
    False

