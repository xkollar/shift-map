module Main (main) where

import Control.Monad (unless)
import Data.Eq ((==))
import System.Exit (exitFailure)
import System.IO (IO)

import Weeder (weeder)


main :: IO ()
main = do
    warningCount <- weeder ["."]
    unless (warningCount == 0) exitFailure
