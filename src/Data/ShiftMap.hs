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
    , shiftAll
    , shift
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
    ) where

import Prelude ((+), (-), error, pred, succ, undefined)

import Control.Arrow (first, second)
import Data.Bool (Bool(True, False), not)
import Data.Eq ((==))
import Data.Function (($), (.), const, id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ord (Ordering(LT, EQ, GT), compare)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid ((<>))
import Data.Tuple (fst)

import Data.ShiftMap.Internal
    ( Key
    , Balance(LH, BA, RH)
    , ShiftMap(Empty, Node)
    , showTree
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

-- | /O(log(n))/.
shiftAll :: Int -> ShiftMap a -> ShiftMap a
shiftAll 0 = id
shiftAll n = go
  where
    go = \case
        Empty -> Empty
        Node ba k v l r -> Node ba (k+n) v (go l) r

-- | /O(log(n))/.
shift :: Int -> Key -> ShiftMap a -> ShiftMap a
shift 0 = const id
shift n = go
  where
    go !kk = \case
        Empty -> Empty
        Node ba k v l r -> case compare kk k of
            GT -> Node ba k v l (go (kk - k) r)
            _ -> Node ba (k + n) v (go kk l) r

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
{-# INLINE insert #-}
insert :: Key -> a -> ShiftMap a -> ShiftMap a
insert = insertWith const

-- | Unimplemented.
insertWith :: (a -> a -> a) -> Key -> a -> ShiftMap a -> ShiftMap a
insertWith = undefined

-- | Unimplemented.
delete :: Key -> ShiftMap a -> ShiftMap a
delete = undefined

-- Call to fix an inbalance of -2
-- returns True if height of root stayed the same
--     U          V
--    / r        / \
--   V     ->  ll   U
-- ll lr          lr r
rotateRight :: ShiftMap a -> (ShiftMap a, Bool)
rotateRight = \case
    Node LH ku u (Node LH kv v ta tb) tc
        -> (Node BA kv v ta (Node BA (ku - kv) u tb tc), False)
    Node LH ku u (Node BA kv v ta tb) tc
        -> (Node RH kv v ta (Node LH (ku - kv) u tb tc), True)
    --     U         W
    --    /  d      / \
    --   V         V   U
    --  a \       a b c d
    --     W
    --    b  c
    Node LH ku u (Node RH kv v ta (Node bw kw w tb tc)) td
        -> let b1 = if bw == RH then LH else BA
               b2 = if bw == LH then RH else BA
           in (Node BA (kv + kw) w (Node b1 kv v ta tb) (Node b2 (ku - kv - kw) u tc td), False)
    t -> error $ "Unexpected rotateRight:\n" <> showTree t

-- Call to fix an inbalance of 2
-- returns True if height of root stayed the same
rotateLeft :: ShiftMap a -> (ShiftMap a, Bool)
rotateLeft = \case
    Node RH ku u tc (Node RH kv v tb ta)
        -> (Node BA (ku + kv) v (Node BA ku u tc tb) ta, False)
    Node RH ku u tc (Node BA kv v tb ta)
        -> (Node LH (ku + kv) v (Node RH ku u tc tb) ta, True)
    --    U         W
    --   d \       / \
    --      V     U   V
    --     / a   d c b a
    --    W
    --   c b
    Node RH ku u td (Node LH kv v (Node bw kw w tc tb) ta)
        -> let b1 = if bw == RH then LH else BA
               b2 = if bw == LH then RH else BA
           in (Node BA (ku + kw) w (Node b1 ku u td tc) (Node b2 (kv - kw) v tb ta), False)
    t -> error $ "Unexpected rotateLeft:\n" <> showTree t

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
