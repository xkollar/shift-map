{-# LANGUAGE LambdaCase #-}

import Prelude hiding (elem)

type Key = Integer

data ShiftTree
    = Empty
    | Node Key ShiftTree ShiftTree
  deriving (Eq, Show)

toList :: ShiftTree -> [Key]
toList = go 0 []
  where
    go a s = \case
        Empty -> s
        Node v l r -> go a (a+v : go (a+v) s r) l

null :: ShiftTree -> Bool
null = \case
    Empty -> True
    _ -> False

empty :: ShiftTree
empty = Empty

singleton :: Key -> ShiftTree
singleton k = Node k Empty Empty

shiftAll :: Integer -> ShiftTree -> ShiftTree
shiftAll 0 = id
shiftAll n = go
  where
    go = \case
        Empty -> Empty
        Node v l r -> Node (v+n) (go l) r

shift :: Integer -> Key -> ShiftTree -> ShiftTree
shift 0 = const id
shift n = go
  where
    go k = \case
        Empty -> Empty
        Node v l r -> case compare k v of
            GT -> Node v l (go (k - v) r)
            _ -> Node (v + n) (go k l) r

elem ::  Key -> ShiftTree -> Bool
elem k = \case
    Empty -> False
    Node v l r -> case compare k v of
        EQ -> True
        LT -> elem k l
        GT -> elem (k-v) r

insert :: Key -> ShiftTree -> ShiftTree
insert k = \case
    Empty -> singleton k
    t@(Node v l r) -> case compare k v of
        GT -> Node v l (insert (k - v) r)
        LT -> Node v (insert k l) r
        EQ -> t

delete :: Key -> ShiftTree -> ShiftTree
delete k = \case
    Empty -> Empty
    Node v l r -> case compare k v of
        GT -> Node v l (delete (k - v) r)
        LT -> Node v (delete k l) r
        EQ -> case deleteMax l of
            Nothing -> Empty
            Just (v', l') -> Node v' l' (shiftAll (v' - v) r)

deleteMax :: ShiftTree -> Maybe (Key, ShiftTree)
deleteMax = \case
    Empty -> Nothing
    Node v l Empty -> Just (v, l)
    Node v l r -> (\(v', r') -> (v+v', Node v l r')) <$> deleteMax r

shiftInsert :: Key -> ShiftTree -> ShiftTree
shiftInsert k = \case
    Empty -> singleton k
    Node v l r -> case compare k v of
        GT -> Node v l (shiftInsert (k - v) r)
        _ -> Node (succ v) (shiftInsert k l) r

shiftDelete :: Key -> ShiftTree -> ShiftTree
shiftDelete k = \case
    Empty -> Empty
    Node v l r -> case compare k v of
        GT -> Node v l (shiftDelete (k - v) r)
        LT -> Node (pred v) (shiftDelete k l) r
        EQ -> case deleteMax l of
            Nothing -> Empty
            Just (v', l') -> Node v' l' (shiftAll (v' - v - 1) r)
