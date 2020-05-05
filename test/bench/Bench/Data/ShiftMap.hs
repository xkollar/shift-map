module Bench.Data.ShiftMap (benchmarks) where

import Prelude ((*), (-), (^))

import Control.Applicative (pure)
import Data.Function (($), flip)
import Data.Int (Int)
import Data.List (foldl', replicate, zip)
import System.IO (IO)

import qualified Data.IntMap.Strict as IntMap

import Criterion.Main (Benchmark, bench, bgroup, env, nf)

import Data.ShiftMap
    ( ShiftMap
    , empty
    , fromList
    , mapKeysMonotonic
    , shift
    , shiftAll
    , shiftInsert
    , shiftDelete
    , size
    )
import DeepseqInstance ()


setupEnv :: IO (ShiftMap ())
setupEnv = pure fixture2to16

fixture1000 :: ShiftMap ()
fixture1000 = foldl' (\ m n -> shiftInsert n () m) empty [1..1000]

fixture2000 :: ShiftMap ()
fixture2000 = foldl' (\ m n -> shiftInsert n () m) empty [1..2000]

fixture4000 :: ShiftMap ()
fixture4000 = foldl' (\ m n -> shiftInsert n () m) empty [1..4000]

fixture2to16 :: ShiftMap ()
fixture2to16 = foldl' (\ m n -> shiftInsert n () m) empty [1..2^(16::Int)-1]

benchmarks :: Benchmark
benchmarks = env setupEnv $ \ bigExample -> bgroup "Bench.Data.ShiftMap"
    [ bgroup "shiftInsert"
        [ bench "1000" $ nf (foldl' (\ m n -> shiftInsert n () m) empty) [1..1000]
        , bench "2000" $ nf (foldl' (\ m n -> shiftInsert n () m) empty) [1..2000]
        , bench "4000" $ nf (foldl' (\ m n -> shiftInsert n () m) empty) [1..4000]
        ]
    , bgroup "Cont.insert"
        [ bench "1000" $ nf (foldl' (\m n -> IntMap.insert n () m) IntMap.empty) [1..1000]
        , bench "2000" $ nf (foldl' (\m n -> IntMap.insert n () m) IntMap.empty) [1..2000]
        , bench "4000" $ nf (foldl' (\m n -> IntMap.insert n () m) IntMap.empty) [1..4000]
        ]
    , bgroup "shiftDelete"
        [ bench "1000" $ nf (foldl' (flip shiftDelete) fixture1000) (replicate 1000 1)
        , bench "2000" $ nf (foldl' (flip shiftDelete) fixture2000) (replicate 2000 1)
        , bench "4000" $ nf (foldl' (flip shiftDelete) fixture4000) (replicate 4000 1)
        ]
    , bench "shiftInsert on big" $ nf (shiftInsert 1 ()) bigExample
    , bench "shiftDelete on big" $ nf (shiftDelete 1) bigExample
    , bench "size on big" $ nf size bigExample
    , bench "shiftAll on big" $ nf (shiftAll 5) bigExample
    , bench "shift 100 100 on big" $ nf (shift 100 100) bigExample
    , bench "shift -100 100 on big" $ nf (shift (-100) 100) bigExample
    , bench "mapKeysMonotonic 4000" $ nf (mapKeysMonotonic (*2)) fixture4000
    , bench "fromList" $ nf fromList (zip [1..] $ replicate 1000 ())
    , bench "Cont.fromList" $ nf IntMap.fromList (zip [1..] $ replicate 1000 ())
    ]
