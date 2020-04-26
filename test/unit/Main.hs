module Main (main) where

import System.IO (IO)

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Data.ShiftMap (tests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Tests.Data.ShiftMap.tests
    ]
