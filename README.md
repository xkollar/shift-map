Container with efficient shift-insert and shift-delete operations
=================================================================

Initial note: This is somewhat experimental, I have several more ideas
that I one to try.

* This one is right-plus left-id. Would like to try right-plus left-minus.
* This one is using AVL. It would probably make sense use similar
  data stracture as in `containers`?
* Play wiht how much strictness is used. (Which part of the
  data structure would be best to be hept strict so that
  performance is good, but also fusion works?)

Motivation
----------

Imagine a deck of cards. We can ask for n-th card in the deck. When we take
that card out, idexes of all following cards will decrease. Similarly
when we insert card at position m indexes of all cards from position m
will increase. Straightforward array implementation leads to linear complexity
(worst case scenario is inserting at/removing from the very beginning of the deck
will require moving every (remaingin) element). (Analogous issues for list-based
implementation.)

This is an example how this can be made more efficient.

Modifying binary search trees
-----------------------------

We are going to introduce modification of binary search tree that will
allow for search (test whether element is member of the data structure),
insert, and delete. Later we will show that also AVL-trees can be extended
this way, thus getting to efficient operations for insertion, deletion, and lookup.

This will be used as a basis to implementation of structure equivalent to `Map Integer a`.

```hs
type Key = Integer

data ShiftTree
    = Empty
    | Node Integer ShiftTree ShiftTree
```

That is a simple binary tree. What makes it different is how we will represent values stored in it.
* Value in the root (`v`) of the tree represent itself.
* Values in left sub-tree are represented unmodified.
* Values in right sub-tree are represented as lowered by `v`.
* Tree with interpreted values is binary search tree.

```
     v
    / \
   l   r
```

As an example let us look at several possible representation of tree storing values 1,2,3.
Shown are represented (naked) and interpreted (parenthesized) values.

```
    3(3)       3(3)       2(2)       1(1)      1(1)
   /          /          / \          \         \
  2(2)       1(1)     1(1)  1(3)       2(3)      1(2)
 /            \                       /           \
1(1)           1(2)                  1(2)          1(3)
```

It should be straigforward that every sutree of valid ShiftTree is a valid ShiftTree
(even though possibly representing different values than it represented in original subtree,
"shifted" by sum of values of nodes from root at which we went turned right).

### Decoding

Retrieving all values in order. O(n) where n is number of values stored.

```hs
toList :: ShiftTree -> [Key]
toList = go 0 []
  where
    go a s = \case
        Empty -> s
        Node v l r -> go a (a+v : go (a+v) s r) l
```

### Basics

Test for emptyness and creating singleton `ShiftTree`. O(1)

```
null :: ShiftTree -> Bool
null = \case
    Empty -> True
    _ -> False

singleton :: Key -> ShiftTree
singleton k = Node k Empty Empty
```

### Shift

The whole tree can be "shifted" (all represented values increased by given number). O(h) where h is height of the tree.

```hs
shiftAll :: Integer -> ShiftTree -> ShiftTree
shiftAll 0 = id
shiftAll n = go
  where
    go = \case
        Empty -> Empty
        Node v l r -> Node (v+n) (go l) r
```

Also part of the tree can be "shifted". O(h) where h is height of the tree.

```hs
shift :: Integer -> Key -> ShiftTree -> ShiftTree
shift 0 = const id
shift n = go
  where
    go k = \case
        Empty -> Empty
        Node v l r -> case compare k v of
            GT -> Node v l (go (k - v) r)
            _ -> Node (v + n) (go k l) r
```

### Lookup

Test whether element is in given `ShiftTree`. O(h) where h is height of the tree.

```hs
elem ::  Key -> ShiftTree -> Bool
elem k = \case
    Empty -> False
    Node v l r -> case compare k v of
        EQ -> True
        LT -> elem k l
        GT -> elem (k-v) r
```

### Insert

Insert element into `ShiftTree`. O(h) where h is height of the tree.

```hs
insert :: Key -> ShiftTree -> ShiftTree
insert k = \case
    Empty -> singleton k
    t@(Node v l r) -> case compare k v of
        GT -> Node v l (insert (k - v) r)
        LT -> Node v (insert k l) r
        EQ -> t
```

### Delete

Delete element into `ShiftTree`. O(h) where h is height of the tree.

```hs
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
```

Shifting version of operations
------------------------------

Existence of these is trivial from existence of `shift`, `insert`, and `delete`.

So far we have provided only basic operations on binary trees (except for shifting the whole tree).
Now we provide `shiftInsert` and `shiftDelete` that will also increase indexes of all elements greater
than or equal to given element.

(For example applying `shiftInsert 1` three times on empty `ShiftTree` and decoding it is expected
to give us `[1,2,3]`. Also note that shift-inserting values 3,2,1 in that order and decoding it
would yield `[1,3,5]`.)

### Shift insert

Inserting with shifting all largrger or equal values by one. O(h) where h is height of the tree.
(Note that we handle equal value as if it was larger value, and it is okay.)
Analogous to simple `insert`, but whenever we are to insert to left sub-tree, we
increase current value by one.

```hs
shiftInsert :: Key -> ShiftTree -> ShiftTree
shiftInsert k = \case
    Empty -> singleton k
    Node v l r -> case compare k v of
        GT -> Node v l (shiftInsert (k - v) r)
        _ -> Node (succ v) (shiftInsert k l) r
```

### Shift delete

Deleting with shifting all larger or equal values by one. O(h) where h is height of the tree.
Again, analogous to simple delete case. (We are even re-useing `deleteMax` from simple `delete` case.).

```hs
shiftDelete :: Key -> ShiftTree -> ShiftTree
shiftDelete k = \case
    Empty -> Empty
    Node v l r -> case compare k v of
        GT -> Node v l (shiftDelete (k - v) r)
        LT -> Node (pred v) (shiftDelete k l) r
        EQ -> case deleteMax l of
            Nothing -> Empty
            Just (v', l') -> Node v' l' (shiftAll (v' - v - 1) r)
```

Limitting the hight of the tree
-------------------------------

It is quite straigtforward that it is possible to use these modifications also on AVL-trees.
Thus complexities of aformentioned operations are to be modified so that `h` is replaced with `log(n)`.

Provided library is such an implementation, and turning it into map-like structure.
