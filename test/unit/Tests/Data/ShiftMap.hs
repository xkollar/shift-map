module Tests.Data.ShiftMap (tests) where

import Data.Function (($), (.), flip)
import Data.Functor (fmap)
import Data.List (cycle, foldr, map, replicate, take)
import Data.Monoid ((<>))
import Data.Maybe (Maybe(Just))
import Data.Tuple (fst)
import Text.Show (show)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Data.ShiftMap
    ( Key
    , ShiftMap
    , empty
    , lookupMin
    , lookupMax
    , shiftAll
    , shiftDelete
    , shiftInsert
    , toList
    )

shiftInsertR :: [Key] -> ShiftMap ()
shiftInsertR = foldr ins empty
  where
    ins k = shiftInsert k ()

keys :: ShiftMap a -> [Key]
keys = map fst . toList

insertTest :: [Key] -> [Key] -> TestTree
insertTest i exp = testCase ("shiftInsert " <> show i) $ proc i @?= exp
  where
    proc = keys . shiftInsertR

example15 :: ShiftMap ()
example15 = shiftInsertR $ replicate 15 1

example1000 :: ShiftMap ()
example1000 = shiftInsertR $ replicate 1000 1

example10000 :: ShiftMap ()
example10000 = shiftInsertR $ replicate 10000 1

exampleCycle :: ShiftMap ()
exampleCycle = foldr (\n -> shiftDelete 16 . shiftInsert n ()) empty . take 10000 $ cycle [1..15]

tests :: TestTree
tests = testGroup "Tests.Data.ShiftMap"
    [ insertTest [2,3,5] [2,4,7]
    , insertTest [4,3,5] [3,4,7]
    , insertTest [4,5,3] [3,4,6]
    , insertTest [6,5,3] [3,5,6]
    , insertTest [1,1,1] [1,2,3]
    , insertTest [12,13,11] [11,12,14]
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
        $ keys (foldr shiftDelete example15 (replicate 16 1)) @?= []
    , testCase "shiftDelete 15 all back"
        $ keys (foldr shiftDelete example15 [0..16]) @?= []
    , testCase "shiftAll 15" $ keys (shiftAll 5 example15) @?= [6..20]
    , testCase "Big shiftInsert" $ keys example10000 @?= [1..10000]
    , testCase "Big shiftDelete" $ keys (foldr shiftDelete example10000 [0..10001]) @?= []
    , testCase "Cycle" $ keys exampleCycle @?= [1..15]
    ]
