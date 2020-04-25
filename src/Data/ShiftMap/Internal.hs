{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.ShiftMap.Internal where

import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Int (Int)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show)

data Balance = LH | BA | RH
  deriving (Generic, Show, Eq)

type Key = Int

data ShiftMap a
    = Empty
    | Node {-# UNPACK #-} !Balance {-# UNPACK #-} !Key !a !(ShiftMap a) !(ShiftMap a)
  deriving (Functor, Generic, Generic1, Show)


