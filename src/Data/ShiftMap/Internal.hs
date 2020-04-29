{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Data.ShiftMap.Internal where

import Prelude ((+), (-), undefined)

import Data.Bool (Bool(True), (&&))
import Data.Eq (Eq, (==))
import Data.Function (($), (.), id, on)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List (map, replicate)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Monoid ((<>))
import Data.Ord ((<), (>), max)
import Data.String (String)
import Data.Tuple (fst)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show, show, showList, showParen, showsPrec)


data Balance = LH | BA | RH
  deriving (Generic, Show, Eq)

type Key = Int

data ShiftMap a
    = Empty
    | Node !Balance {-# UNPACK #-} !Key !a !(ShiftMap a) !(ShiftMap a)
  deriving (Functor, Generic, Generic1)

instance Eq a => Eq (ShiftMap a) where
    {-# INLINE (==) #-}
    (==) = (==) `on` toList

instance Show a => Show (ShiftMap a) where
    showsPrec n m = showParen (n > 10) $ \s ->
        "fromList " <> showList (toList m) s

-- | /O(n)/.
depth :: ShiftMap a -> Int
depth = \case
    Empty -> 0
    Node _ _ _ l r -> 1 + max (depth l) (depth r)

-- | /O(n)/.
toList :: ShiftMap a -> [(Key, a)]
toList t = go 0 t []
  where
    go _ Empty = id
    go n (Node _ k v l r) = go n l . ((c, v):) . go c r
      where
        c = n + k

-- | Unimplemented.
fromList :: [(Key, a)] -> ShiftMap a
fromList = undefined

fromAscList :: [(Key, a)] -> ShiftMap a
fromAscList = undefined

-- | Show for debugging purposes (discards stored values)
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

-- | Validity test.
valid :: ShiftMap a -> Bool
valid m = searchTree m && avlTree m
  where
    keys = map fst . toList
    isAsc = \case
        (x:t@(y:_)) ->  x < y && isAsc t
        _ -> True
    searchTree = isAsc . keys
    avlTest :: ShiftMap a -> Maybe Int
    avlTest = \case
        Empty -> Just 0
        Node ba _ _ l r -> do
            rl <- avlTest l
            rr <- avlTest r
            case (rl - rr, ba) of
                (1, LH) -> Just (rl+1)
                (0, BA) -> Just (rl+1)
                (-1, RH) -> Just (rr+1)
                _ -> Nothing
    avlTree = isJust . avlTest
