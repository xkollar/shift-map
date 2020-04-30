module Tests.Data.ShiftMap (tests) where

import Prelude ((*), (+))

import Data.Function (($), (.), flip)
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (cycle, dropWhile, foldl', head, map, replicate, reverse, tail, take, zip, zipWith)
import Data.Monoid ((<>))
import Data.Maybe (Maybe(Just))
import Data.Ord ((<))
import Data.Tuple (fst, snd)
import Text.Show (show)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

import Data.ShiftMap
    ( Key
    , ShiftMap
    , empty
    , lookupMin
    , lookupMax
    , shift
    , shiftAll
    , shiftDelete
    , shiftInsert
    , size
    , toList
    , mapKeysMonotonic
    )
import Data.ShiftMap.Internal (depth, valid)

shiftInsertL :: [Key] -> ShiftMap ()
shiftInsertL = foldl' ins empty
  where
    ins m k = shiftInsert k () m

keys :: ShiftMap a -> [Key]
keys = map fst . toList

insertTest :: [Key] -> [Key] -> TestTree
insertTest i exp = testCase ("shiftInsert " <> show i) $ proc i @?= exp
  where
    proc = keys . shiftInsertL

maxDepth :: Int -> Int
maxDepth n = snd . head . dropWhile ((<n) . fst) $ zip s [0..]
  where
    s = 0:1: zipWith (\x y -> x+y+1) s (tail s)

example15 :: ShiftMap ()
example15 = shiftInsertL $ replicate 15 1

example1000 :: ShiftMap ()
example1000 = shiftInsertL $ replicate 1000 1

example10000 :: ShiftMap ()
example10000 = shiftInsertL $ replicate 10000 1

exampleCycle :: ShiftMap ()
exampleCycle = foldl' (\ m n -> shiftDelete 16 $ shiftInsert n () m) empty . take 10000 $ cycle [1..15]

tests :: TestTree
tests = testGroup "Tests.Data.ShiftMap"
    [ insertTest [5,3,2] [2,4,7]
    , insertTest [5,3,4] [3,4,7]
    , insertTest [3,5,4] [3,4,6]
    , insertTest [3,5,6] [3,5,6]
    , insertTest [1,1,1] [1,2,3]
    , insertTest [11,13,12] [11,12,14]
    , testCase "lookupMin 15" $ lookupMin example15 @?= Just (1, ())
    , testCase "lookupMax 15" $ lookupMax example15 @?= Just (15, ())
    , testCase "lookupMin 1000" $ lookupMin example1000 @?= Just (1, ())
    , testCase "lookupMax 1000" $ lookupMax example1000 @?= Just (1000, ())
    , testGroup "shiftDelete 15" . flip fmap [1..15] $ \n -> testCase (show n)
        $ keys (shiftDelete n example15) @?= [1..14]
    , testCase "shiftDelete 15 0"
        $ keys (shiftDelete 0 example15) @?= [0..14]
    , testCase "shiftDelete 15 16"
        $ keys (shiftDelete 16 example15) @?= [1..15]
    , testCase "shiftDelete 15 all front"
        $ keys (foldl' (flip shiftDelete) example15 (replicate 16 1)) @?= []
    , testCase "shiftDelete 15 all back"
        $ keys (foldl' (flip shiftDelete) example15 (reverse [0..16])) @?= []
    , testCase "Big shiftInsert" $ keys example10000 @?= [1..10000]
    , testCase "Big shiftDelete" $ keys (foldl' (flip shiftDelete) example10000 [10001,10000..0]) @?= []
    , testCase "Cycle" $ keys exampleCycle @?= [1..15]
    , testCase "Big size" $ size example10000 @?= 10000
    , testGroup "Valid"
        [ testCase "15" $ assertBool "Invalid example!" (valid example15)
        , testCase "1000" $ assertBool "Invalid example!" (valid example1000)
        , testCase "Big" $ assertBool "Invalid example!" (valid example10000)
        , testCase "Cycle" $ assertBool "Invalid example!" (valid exampleCycle)
        ]
    , testGroup "Depth"
        [ testCase "bound 1000" $ assertBool "Depth is above expected bound" (depth example10000 < maxDepth 10000)
        , testCase "exact 1000" $ depth example10000 @?= 14
        ]
    , testGroup "Shift"
        [ testCase "shiftAll 15" $ keys (shiftAll 5 example15) @?= [6..20]
        , testCase "shiftLeft 15" $ keys (shift (-1) 5 example15) @?= [0..4] <> [6..15]
        , testCase "shiftRight 15" $ keys (shift 1 5 example15) @?= [1..4] <> [6..16]
        ]
    , testGroup "MapKeys" $ let m = mapKeysMonotonic (*2) example1000 in
        [ testCase "valid" $ assertBool "Invalid!" (valid m)
        , testCase "keys" $ keys m @?= [2,4..2000]
        ]
    ]
