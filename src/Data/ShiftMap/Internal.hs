{-# LANGUAGE DeriveFunctor #-}
module Data.ShiftMap.Internal where

import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Int (Int)
import Text.Show (Show)

data Balance = LH | BA | RH deriving (Show, Eq)

type Key = Int

data ShiftMap a
    = Empty
    | Node !Balance !Key !a (ShiftMap a) (ShiftMap a)
  deriving (Functor, Show)

