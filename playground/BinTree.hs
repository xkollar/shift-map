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
