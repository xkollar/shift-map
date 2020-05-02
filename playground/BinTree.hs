{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module BinTree where

import Svg

data BinTree a
    = E
    | N a (BinTree a) (BinTree a)
  deriving (Functor, Show)

root :: BinTree a -> Maybe a
root = \case
    E -> Nothing
    N v _ _ -> Just v

getLeft :: BinTree a -> Maybe (BinTree a)
getLeft = \case
    E -> Nothing
    N _ l _ -> Just l

getRight :: BinTree a -> Maybe (BinTree a)
getRight = \case
    E -> Nothing
    N _ _ r -> Just r

data Balance = LH | BA | RH
  deriving (Eq, Show)

size :: BinTree a -> Int
size = go 0
  where
    go !n = \case
        E -> n
        N _ l r -> go (go (succ n) l) r

depth :: BinTree a -> Int
depth = \case
    E -> 0
    N _ l r -> 1 + max (depth l) (depth r)

inOrder :: BinTree a -> [a]
inOrder t = go t []
  where
    go = \case
        E -> id
        N v l r -> go l . (v:) . go r

mkDepth :: Int -> BinTree ()
mkDepth = \case
    0 -> E
    n -> let t = mkDepth (pred n)
        in N () t t

split :: Int -> (Int, Int)
split n = (d + m, d)
  where
    (d, m) = divMod (pred n) 2

mkNSplit :: Int -> BinTree ()
mkNSplit = \case
    0 -> E
    n -> let (l,r) = split n
        in N () (mkNSplit l) (mkNSplit r)

prepare :: Int -> [(Int, Int)]
prepare x = go [] 0 1 0
  where
    go s n p pp
        | pp > x = s
        | otherwise = let p' = p * 2 in go ((n, pp):s) (succ n) p' (pred p')

-- TODO: Make nicer
mkAligned :: Int -> BinTree Balance
mkAligned 0 = E
mkAligned n = start $ prepare n
  where
    start ((dep, opt):s) = go dep (n - opt) s
    start _ = error "Oops!"

    go cdep 0    _ = fmap (const BA) $ mkDepth cdep
    go cdep dang ((dep, opt):s) = if dang <= opt
        then N LH (go dep  dang s) (go dep 0 s)
        else N (if dang - opt - 1 == 0 then LH else BA) (go cdep 0    s) (go dep (dang - opt - 1) s)
    go _ _ _ = error "Oops!"

label :: [a] -> BinTree b -> BinTree a
label ls = fst . go ls
  where
    go s = \case
        E -> (E, s)
        N _ l r -> let
            (l', v:s') = go s l
            (r', s'') = go s' r
            in (N v l' r', s'')

label2 :: [a] -> BinTree b -> BinTree a
label2 = go (\ t l -> if null l then t else error "Oops!")
  where
    go f s = \case
        E -> f E s
        N _ l r -> go (\ l' (x:rl) -> go (\ r' rr -> f (N x l' r') rr) rl r) s l

fromListWith :: (Int -> BinTree a) -> [b] -> BinTree b
fromListWith f s = label s (f (length s))

fromListWith2 :: (Int -> BinTree a) -> [b] -> BinTree b
fromListWith2 f s = label2 s (f (length s))

countInbalanced :: BinTree a -> Int
countInbalanced = length . filter (/=BA) . inOrder . putBalance

--
--
--

class Drawable a where
    draw :: a -> Int -> Svg

instance Drawable () where
    draw _ n = tag "circle" [("r", show n), ("fill", "#fff")]

instance Drawable Balance where
    draw x n = tag "circle" [("r", show n), ("fill", c)]
      where
        c = case x of
            LH -> "red"
            BA -> "black"
            RH -> "green"

instance Drawable Int where
    draw x n = bg . text
      where
        bg = tag "circle" [("r", show n), ("fill", "#fff")]
        text = pairTag "text" textAttrs $ (show x <>)
        textAttrs =
            [ ("stroke", "none")
            , ("fill", "#000")
            , ("text-anchor", "middle")
            , ("dominant-baseline", "middle")
            ]

instance Drawable String where
    draw t n = bg . text
      where
        l = length t * n * 3 `div` 4
        bg = tag "rect" [("x", show (-l)), ("y", "-10"), ("width", show (l*2)), ("height", "20"), ("fill", "#fff")]
        text = pairTag "text" textAttrs $ (t <>)
        textAttrs =
            [ ("stroke", "none")
            , ("fill", "#000")
            , ("text-anchor", "middle")
            , ("dominant-baseline", "middle")
            ]

putDepths :: BinTree a -> BinTree Int
putDepths = fst . go
  where
    go = \case
        E -> (E, 0)
        N _ l r -> let
            (l', dl) = go l
            (r', dr) = go r
            d = 1 + max dl dr
            in (N d l' r', d)

putBalance :: BinTree a -> BinTree Balance
putBalance = fst . go
  where
    go = \case
        E -> (E, 0 :: Int)
        N _ l r -> let
            (l', dl) = go l
            (r', dr) = go r
            d = 1 + max dl dr
            ba = case compare dl dr of
                GT -> LH
                EQ -> BA
                LT -> RH
            in (N ba l' r', d)


renderTree90 :: Drawable a => BinTree a -> Svg
renderTree90 = go id
  where
    -- go :: (Svg -> Svg) -> BinTree a -> Svg
    go wrap = \case
        E -> empty
        N a l r -> wrap (go
                (\ svg -> group [("transform", "rotate(90)")]
                    $ line (0,0) (0,100)
                    . group [("transform", "translate(0 100) scale(0.7) rotate(0)")] svg)
                l
            . go
                (\ svg -> group [("transform", "rotate(-90)")]
                    $ line (0,0) (0,100)
                    . group [("transform", "translate(0 100) scale(0.7) rotate(0)")] svg)
                r
            . draw a 10)

renderTree45 :: Drawable a => BinTree a -> Svg
renderTree45 = go id
  where
    -- go :: (Svg -> Svg) -> BinTree a -> Svg
    go wrap = \case
        E -> empty
        N a l r -> wrap (go
                (\ svg -> group [("transform", "rotate(45)")]
                    $ line (0,0) (0,100)
                    . group [("transform", "translate(0 100) scale(0.499) rotate(-45)")] svg)
                l
            . go
                (\ svg -> group [("transform", "rotate(-45)")]
                    $ line (0,0) (0,100)
                    . group [("transform", "translate(0 100) scale(0.499) rotate(45)")] svg)
                r
            . draw a 10)

writeTreeSvgWith :: Drawable a => (BinTree a -> Svg) -> FilePath -> BinTree a -> IO ()
writeTreeSvgWith render path t = writeSvg 400 400 path
    $ (fill "none" . stroke "black") (rect 400 400)
    . group
        [ ("transform","translate(200 200)")
        , ("fill", "#fff")
        , ("stroke","#000")
        ]
        (render t)
