{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Data.ShiftMap.Internal where

import Prelude ((+))

import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List (replicate)
import Data.Monoid ((<>))
import Data.Ord (max)
import Data.String (String)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show, show)


data Balance = LH | BA | RH
  deriving (Generic, Show, Eq)

type Key = Int

data ShiftMap a
    = Empty
    | Node {-# UNPACK #-} !Balance {-# UNPACK #-} !Key !a !(ShiftMap a) !(ShiftMap a)
  deriving (Functor, Generic, Generic1, Show)

-- | Debug
showTree :: ShiftMap a -> String
showTree = go 0 0
  where
    go dk n (Node b k _ Empty Empty) = sp n <> "Node " <> show b <> " " <> show k <> "(" <> show (dk + k) <> ") _"
    go dk n (Node b k _ l r) = sp n <> "Node " <> show b <> " " <> show k <> "(" <> show (dk + k) <> ") _"
        <> go dk (n+4) l
        <> go (dk+k) (n+4) r
    go _ n Empty = sp n <> "Empty"
    sp 0 = ""
    sp n = "\n" <> replicate n ' '

depth :: ShiftMap a -> Int
depth = \case
    Empty -> 0
    Node _ _ _ l r -> 1 + max (depth l) (depth r)
