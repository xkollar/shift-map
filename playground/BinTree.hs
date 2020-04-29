{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module BinTree where

import Svg

data BinTree
    = E
    | N BinTree BinTree
  deriving Show

size :: BinTree -> Int
size = go 0
  where
    go !n = \case
        E -> n
        N l r -> go (go (succ n) l) r

depth :: BinTree -> Int
depth = \case
    E -> 0
    N l r -> 1 + max (depth l) (depth r)

mkDepth :: Int -> BinTree
mkDepth = \case
    0 -> E
    n -> let t = mkDepth (pred n)
        in N t t

split :: Int -> (Int, Int)
split n = (d + m, d)
  where
    (d, m) = divMod (pred n) 2

mkNSplit :: Int -> BinTree
mkNSplit = \case
    0 -> E
    n -> let (l,r) = split n
        in N (mkNSplit l) (mkNSplit r)

renderTree90 :: BinTree -> Svg
renderTree90 = go id
  where
    go :: (Svg -> Svg) -> BinTree -> Svg
    go wrap = \case
        E -> empty
        N l r -> wrap (go
                (\ svg -> g [("transform", "rotate(90)")]
                    $ line (0,0) (0,100)
                    . g [("transform", "translate(0 100) scale(0.7) rotate(0)")] svg)
                l
            . go
                (\ svg -> g [("transform", "rotate(-90)")]
                    $ line (0,0) (0,100)
                    . g [("transform", "translate(0 100) scale(0.7) rotate(0)")] svg)
                r
            . circle 10)

renderTree45 :: BinTree -> Svg
renderTree45 = go id
  where
    go :: (Svg -> Svg) -> BinTree -> Svg
    go wrap = \case
        E -> empty
        N l r -> wrap (go
                (\ svg -> g [("transform", "rotate(45)")]
                    $ line (0,0) (0,100)
                    . g [("transform", "translate(0 100) scale(0.499) rotate(-45)")] svg)
                l
            . go
                (\ svg -> g [("transform", "rotate(-45)")]
                    $ line (0,0) (0,100)
                    . g [("transform", "translate(0 100) scale(0.499) rotate(45)")] svg)
                r
            . circle 10)

writeTreeSvgWith :: (BinTree -> Svg) -> FilePath -> BinTree -> IO ()
writeTreeSvgWith render path t = writeSvg 400 400 path
    $ (fill "none" . stroke "black") (rect 400 400)
    . g
        [ ("transform","translate(200 200)")
        , ("fill", "#fff")
        , ("stroke","#000")
        ]
        (render t)
