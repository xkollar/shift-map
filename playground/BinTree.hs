{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module BinTree where

import Svg

data BinTree a
    = E
    | N a (BinTree a) (BinTree a)
  deriving Show

data Balance = LH | BA | RH
  deriving Show

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

--
--
--

class Colorable a where
    toColor :: a -> Color

instance Colorable () where
    toColor _ = "%fff"

instance Colorable Balance where
    toColor = \case
        LH -> "red"
        BA -> "black"
        RH -> "green"

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


renderTree90 :: Colorable a => BinTree a -> Svg
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
            . mark a)
    mark a = group [("fill", toColor a)] $ circle 10

renderTree45 :: Colorable a => BinTree a -> Svg
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
            . mark a)
    mark a = group [("fill", toColor a)] $ circle 10

writeTreeSvgWith :: Colorable a => (BinTree a -> Svg) -> FilePath -> BinTree a -> IO ()
writeTreeSvgWith render path t = writeSvg 400 400 path
    $ (fill "none" . stroke "black") (rect 400 400)
    . group
        [ ("transform","translate(200 200)")
        , ("fill", "#fff")
        , ("stroke","#000")
        ]
        (render t)
