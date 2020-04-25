module Bench.Data.ShiftMap (benchmarks) where

import Prelude ((^), (-))

import Control.Applicative (pure)
import Data.Function (($), flip)
import Data.Int (Int)
import Data.List (foldl', replicate)
import System.IO (IO)

import Criterion.Main (Benchmark, bench, bgroup, env, nf)

import Data.ShiftMap
    ( ShiftMap
    , empty
    , shiftInsert
    , shiftDelete
    , size
    )

import DeepseqInstance ()


setupEnv :: IO (ShiftMap ())
setupEnv = pure fixture2to16

fixture1000 :: ShiftMap ()
fixture1000 = foldl' (\ m n -> shiftInsert n () m) empty [1..1000]

fixture2to16 :: ShiftMap ()
fixture2to16 = foldl' (\ m n -> shiftInsert n () m) empty [1..2^(16::Int)-1]

benchmarks :: Benchmark
benchmarks = env setupEnv $ \ bigExample -> bgroup "Bench.Data.ShiftMap"
    [ bench "1000 shiftInsert" $ nf (foldl' (\ m n -> shiftInsert n () m) empty) [1..1000]
    , bench "1000 shiftDelete" $ nf (foldl' (flip shiftDelete) fixture1000) (replicate 1000 1)
    , bench "shiftInsert on big" $ nf (shiftInsert 1 ()) bigExample
    , bench "shiftDelete on big" $ nf (shiftDelete 1) bigExample
    , bench "size on big" $ nf size bigExample
    ]
