module Main (main) where

import System.IO (IO)

import Criterion.Main (defaultMain)

import qualified Bench.Data.ShiftMap (benchmarks)

main :: IO ()
main = defaultMain
    [ Bench.Data.ShiftMap.benchmarks
    ]
