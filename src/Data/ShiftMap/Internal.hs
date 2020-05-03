{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Data.ShiftMap.Internal where

import Prelude ((*), (+), (-), error, pred, succ)

import Data.Bool (Bool(False, True), (&&), otherwise)
import Data.Eq (Eq, (==))
import Data.Function (($), (.), const, id, on)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List (foldl', length, map, replicate)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Monoid ((<>))
import Data.Ord (Ord, (<), (<=), (>), Ordering(EQ, GT, LT), compare, max)
import Data.String (String)
import Data.Tuple (fst)
import GHC.Generics (Generic, Generic1)
import Text.Read (Read, lex, readList, readParen, readsPrec)
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

instance Read a => Read (ShiftMap a) where
    readsPrec d = readParen (d > 10) $ \r ->
        [(fromList m, t) | ("fromList", s) <- lex r, (m, t) <- readList s]

-- | /O(n)/.
depth :: ShiftMap a -> Int
depth = \case
    Empty -> 0
    Node _ _ _ l r -> 1 + max (depth l) (depth r)

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
insertWith' :: (a -> a -> a) -> Key -> a -> ShiftMap a -> (ShiftMap a, Bool)
insertWith' f = go
  where
    go v x = \case
        Empty -> (Node BA v x Empty Empty, True)
        Node b w wx tl tr -> case compare v w of
          GT -> let (tr', isHigher) = go (v - w) x tr in case (isHigher, b) of
            (False, _) ->            (Node b  w wx tl tr', False)
            (True, LH) ->            (Node BA w wx tl tr', False)
            (True, BA) ->            (Node RH w wx tl tr', True)
            (True, RH) -> rotateLeft (Node RH w wx tl tr')
          LT -> let (tl', isHigher) = go v x tl in case (isHigher, b) of
            (False, _) ->             (Node b  w wx tl' tr, False)
            (True, RH) ->             (Node BA w wx tl' tr, False)
            (True, BA) ->             (Node LH w wx tl' tr, True)
            (True, LH) -> rotateRight (Node LH w wx tl' tr)
          EQ -> (Node b w (f x wx) tl tr, False)

-- | /O(log(n))/.
insertWith :: (a -> a -> a) -> Key -> a -> ShiftMap a -> ShiftMap a
insertWith f v k = fst . insertWith' f v k

-- | Transitively unimplemented.
{-# INLINE insert #-}
insert :: Key -> a -> ShiftMap a -> ShiftMap a
insert = insertWith const

-- | /O(n)/.
toList :: ShiftMap a -> [(Key, a)]
toList t = go 0 t []
  where
    go _ Empty = id
    go n (Node _ k v l r) = go n l . ((c, v):) . go c r
      where
        c = n + k

-- | Transitively unimplemented.
fromList :: [(Key, a)] -> ShiftMap a
fromList s = case ascLength (map fst s) of
    Nothing -> foldl' (\ m (k,v) -> insert k v m) Empty s
    Just n -> fromAscListOfLength n s

-- | Transitively unimplemented.
fromAscList :: [(Key, a)] -> ShiftMap a
fromAscList s = fromAscListOfLength (length s) s

-- | Unimplemented.
fromAscListOfLength :: Int -> [(Key, a)] -> ShiftMap a
fromAscListOfLength n s = labelSkeleton s $ skeletonOfSize n

-- | Internal.
fromAscListOfDepth :: Int -> [(Key, a)] -> (ShiftMap a, [(Key, a)])
fromAscListOfDepth = go 0
  where
    -- TODO: Uggh, please rewrite.
    go _ 0 s = (Empty, s)
    go d !n s = (Node BA (k-d) v l r, rest)
      where
        (l, ls) = go d (n-1) s
        (r, rest) = go k (n-1) ls'
        (k, v, ls') = case ls of
            (k', v'):rs -> (k', v', rs)
            [] -> error "Unexpected labelledOfDepth: []"

skeletonOfDepth :: Int -> ShiftMap ()
skeletonOfDepth = go
  where
    go = \case
        0 -> Empty
        n -> let t = go (pred n)
            in Node BA 0 () t t

skeletonOfSize :: Int -> ShiftMap ()
skeletonOfSize len = init [] 0 1 0
  where
    init s n p pp
        | pp > len = case s of
            (dep, opt):s' -> go dep (len - opt) s'
            [] -> error "Unexpected fromAscListOfLength: []"
        | otherwise = let p' = p * 2 in init ((n, pp):s) (succ n) p' (pred p')

    go cdep 0    _ = skeletonOfDepth cdep
    go cdep dang ((dep, opt):s) = if dang <= opt
        then Node LH 0 () (go dep  dang s) (go dep 0 s)
        else Node (if dang - opt - 1 == 0 then LH else BA) 0 () (go cdep 0    s) (go dep (dang - opt - 1) s)
    go _ _ _ = error "Unexpected fromAscListOfLength: init failed to provide enough input?"

labelSkeleton' :: [(Key, a)] -> ShiftMap () -> (ShiftMap a, [(Key, a)])
labelSkeleton'= go 0
  where
    go !n s = \case
        Empty -> (Empty, s)
        Node b _ _ l r -> let
            (l', (k, v):s') = go n s l
            (r', s'') = go k s' r
            in (Node b (k-n) v l' r', s'')

labelSkeleton :: [(Key, a)] -> ShiftMap () -> ShiftMap a
labelSkeleton s skel = case labelSkeleton' s skel of
    (m, []) -> m
    _ ->  error "Unexpected labelSkeleton: unconsumed labels"

-- Internal. Combine test for monotonicity with finding out length.
ascLength :: Ord a => [a] -> Maybe Int
ascLength = go 0
  where
    go !n = \case
        (x:s@(y:_)) -> if x < y
            then go (succ n) s
            else Nothing
        [_] -> Just (succ n)
        [] -> Just n

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
