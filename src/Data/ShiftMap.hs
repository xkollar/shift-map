{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Data.ShiftMap
    ( Key
    , ShiftMap
    , empty
    , singleton
    , null
    , size
    , toList
    , fromList
    , fromAscList
    , shift
    , shiftAll
    , lookup
    , lookupMin
    , lookupMax
    , findMin
    , findMax
    , insert
    , insertWith
    , shiftInsert
    , delete
    , shiftDelete
    , adjust
    , mapKeysMonotonic
    ) where

import Prelude ((+), (-), error, pred, succ, undefined)

import Control.Arrow (first, second)
import Data.Bool (Bool(True, False), not)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord (Ordering(LT, EQ, GT), compare)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (fst)

import Data.ShiftMap.Internal
    ( Key
    , Balance(LH, BA, RH)
    , ShiftMap(Empty, Node)
    , fromList
    , fromAscList
    , insert
    , insertWith
    , rotateLeft
    , rotateRight
    , toList
    )

-- | /O(1)/.
{-# INLINE empty #-}
empty :: ShiftMap a
empty = Empty

-- | /O(1)/.
{-# INLINE singleton #-}
singleton :: Key -> a -> ShiftMap a
singleton k v = Node BA k v Empty Empty

-- | /O(1)/.
{-# INLINE null #-}
null :: ShiftMap a -> Bool
null = \case
    Empty -> True
    _ -> False

-- | /O(n)/.
size :: ShiftMap a -> Int
size = go 0
  where
    go !n = \case
        Empty -> n
        Node _ _ _ l r -> go (go (succ n) l) r

-- | /O(log(n))/. Increase all keys in map by /n/ for any /n/.
shiftAll :: Int -> ShiftMap a -> ShiftMap a
shiftAll 0 = id
shiftAll n = go
  where
    go = \case
        Empty -> Empty
        Node ba k v l r -> Node ba (k+n) v (go l) r

-- | /O(log(n))/. Increase all keys /<= k/ by /n/. Assumes /n <= 0/.
shiftLeft :: Int -> Key -> ShiftMap a -> ShiftMap a
shiftLeft n k = shiftRight (-n) k . shiftAll n

-- | /O(log(n))/. Increase all keys />= k/ by /n/. Assumes /n >= 0/.
shiftRight :: Int -> Key -> ShiftMap a -> ShiftMap a
shiftRight n = go
  where
    go !kk = \case
        Empty -> Empty
        Node ba k v l r -> case compare kk k of
            GT -> Node ba k v l (go (kk - k) r)
            _ -> Node ba (k + n) v (go kk l) r

-- | /O(log(n))/. Applic @shift n k m@:
-- * If @n == 0@ returns @m@.
-- * If @n < 0@ shifts all keys @<= k@ by @n@ in @m@.
-- * If @n > 0@ shifts all keys @>= k@ by @n@ in @m@.
-- Hmmmmm… this shift can't be used for shiftelete as combination shift and delete :-/.
shift :: Int -> Key -> ShiftMap a -> ShiftMap a
shift n = case compare n 0 of
    LT -> shiftLeft n
    EQ -> const id
    GT -> shiftRight n

-- | /O(log(n))/.
lookupMin :: ShiftMap a -> Maybe (Key, a)
lookupMin = \case
    Empty -> Nothing
    Node _ k v Empty _ -> Just (k, v)
    Node _ _ _ l _ -> lookupMin l

-- | /O(log(n))/.
lookupMax :: ShiftMap a -> Maybe (Key, a)
lookupMax = \case
    Empty -> Nothing
    Node _ k v _ Empty -> Just (k, v)
    Node _ k _ _ r -> first (k+) <$> lookupMax r

-- | /O(log(n))/.
{-# INLINE findMax #-}
findMax :: ShiftMap a -> (Key, a)
findMax = fromMaybe (error "findMax: empty") . lookupMax

-- | /O(log(n))/.
{-# INLINE findMin #-}
findMin :: ShiftMap a -> (Key, a)
findMin = fromMaybe (error "findMin: empty") . lookupMin

-- | /O(log(n))/.
lookup :: Key -> ShiftMap a -> Maybe a
lookup kk = \case
    Empty -> Nothing
    Node _ k v l r -> case compare kk k of
        GT -> lookup (kk - k) r
        LT -> lookup kk l
        EQ -> Just v

-- | Unimplemented.
delete :: Key -> ShiftMap a -> ShiftMap a
delete = undefined

-- returns True if the height increased
shiftInsert' :: Key -> a -> ShiftMap a -> (ShiftMap a, Bool)
shiftInsert' v x = \case
    Empty -> (Node BA v x Empty Empty, True)
    Node b w wx tl tr -> case compare v w of
      GT -> let (tr', isHigher) = shiftInsert' (v - w) x tr in case (isHigher, b) of
        (False, _) ->            (Node b  w wx tl tr', False)
        (True, LH) ->            (Node BA w wx tl tr', False)
        (True, BA) ->            (Node RH w wx tl tr', True)
        (True, RH) -> rotateLeft (Node RH w wx tl tr')
      _ -> let w' = succ w; (tl', isHigher) = shiftInsert' v x tl in case (isHigher, b) of
        (False, _) ->             (Node b  w' wx tl' tr, False)
        (True, RH) ->             (Node BA w' wx tl' tr, False)
        (True, BA) ->             (Node LH w' wx tl' tr, True)
        (True, LH) -> rotateRight (Node LH w' wx tl' tr)

-- | /O(log(n))/.
shiftInsert :: Key -> a -> ShiftMap a -> ShiftMap a
shiftInsert v k = fst . shiftInsert' v k

-- returns True if the height decreased
shiftDelete' :: Key -> ShiftMap k -> (ShiftMap k, Bool)
shiftDelete' _ Empty = (Empty, False)
shiftDelete' v (Node b w wx tl tr) = case compare v w of
  LT -> let w' = pred w;(tl', isSmaller) = shiftDelete' v tl in case (isSmaller, b) of
    (False, _) ->                         (Node b  w' wx tl' tr, False)
    (True, LH) ->                         (Node BA w' wx tl' tr, True)
    (True, BA) ->                         (Node RH w' wx tl' tr, False)
    (True, RH) -> second not $ rotateLeft (Node RH w' wx tl' tr)
  GT -> let (tr', isSmaller) = shiftDelete' (v - w) tr in case (isSmaller, b) of
    (False, _) ->                          (Node b  w wx tl tr', False)
    (True, RH) ->                          (Node BA w wx tl tr', True)
    (True, BA) ->                          (Node LH w wx tl tr', False)
    (True, LH) -> second not $ rotateRight (Node LH w wx tl tr')
  EQ -> case tl of
    Empty -> (tr, True)
    _     ->
        let -- (tl', isSmaller, u) = deleteMaximum tl
            (mk, x) = findMax tl
            (tl', isSmaller) = shiftDelete' mk tl
            tr' = shiftAll (w - mk - 1) tr
        in case (isSmaller, b) of
          (False, _) ->                         (Node b  mk x tl' tr', False)
          (True, LH) ->                         (Node BA mk x tl' tr', True)
          (True, BA) ->                         (Node RH mk x tl' tr', False)
          (True, RH) -> second not $ rotateLeft (Node RH mk x tl' tr')

-- | /O(log(n))/.
shiftDelete :: Key -> ShiftMap a -> ShiftMap a
shiftDelete v = fst . shiftDelete' v

-- | /O(log(n))/.
adjust :: Key -> (a -> a) -> ShiftMap a -> ShiftMap a
adjust k f = \case
    Empty -> Empty
    Node d kk v l r -> case compare k kk of
        GT -> Node d kk v l (adjust (k - kk) f r)
        LT -> Node d kk v (adjust k f l) r
        EQ -> Node d kk (f v) l r

mapKeysMonotonic :: (Key -> Key) -> ShiftMap a -> ShiftMap a
mapKeysMonotonic f = go 0 0
  where
    go !old !new = \case
        Empty -> Empty
        Node ba k v l r -> let next = f (old + k) - new
            in Node ba next v (go k next l) (go k next r)
