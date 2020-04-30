module Svg where

import Data.Monoid ((<>))

type Svg = ShowS

type Attrs = [(String, String)]

type Point = (Int,Int)

type Color = String

empty :: Svg
empty = id

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

group :: Attrs -> Svg -> Svg
group = pairTag "g"

stroke :: String -> Svg -> Svg
stroke color = group [("stroke", color)]

fill :: String -> Svg -> Svg
fill color = group [("fill", color)]

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
