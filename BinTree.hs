{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module BinTree where

import Data.Monoid ((<>))

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

mkDepth = \case
    0 -> E
    n -> let t = mkDepth (pred n)
        in N t t

split :: Int -> (Int, Int)
split n = (d + m, d)
  where
    (d, m) = divMod (pred n) 2

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

-------------------

type Svg = ShowS

empty :: Svg
empty = id

type Attrs = [(String, String)]

type Point = (Int,Int)

showAttrs :: Attrs -> ShowS
showAttrs = (\(f,g) -> f . g . f)
    . foldr
        (\(n,v) (sep,s) -> ((' ':), (n<>) . ('=':) . shows v . sep . s))
        (id,id)

pairTag :: String -> Attrs -> Svg -> Svg
pairTag name attrs svg = (('<' : name) <>)
    . showAttrs attrs
    . ('>':)
    . svg
    . (\ s -> '<' : '/' : (name <> ('>' : s)))

tag :: String -> Attrs -> Svg
tag name attrs =
    (('<' : name) <>) . showAttrs attrs . ("/>"<>)

circle :: Int -> Svg
circle r = tag "circle" [("r", show r)]

line :: Point -> Point -> Svg
line (x1, y1) (x2, y2) = tag "line"
    [ ("x1", show x1)
    , ("y1", show y1)
    , ("x2", show x2)
    , ("y2", show y2)
    ]

rect :: Int -> Int -> Svg
rect w h = tag "rect"
    [ ("width", show w)
    , ("height", show h)
    ]

g :: Attrs -> Svg -> Svg
g = pairTag "g"

stroke :: String -> Svg -> Svg
stroke color = g [("stroke", color)]

fill :: String -> Svg -> Svg
fill color = g [("fill", color)]

writeSvg :: Int -> Int -> FilePath -> Svg -> IO ()
writeSvg w h path svg =
    writeFile path $
        pairTag
            "svg"
            [ ("version", "1.1")
            , ("baseProfile", "full")
            , ("width", show w)
            , ("height", show h)
            , ("xmlns", "http://www.w3.org/2000/svg")
            ]
            svg
            ""
