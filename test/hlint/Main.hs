module Main (main) where

import Control.Monad (unless)
import Data.List (null)
import System.Exit (exitFailure)
import System.IO (IO)

import Language.Haskell.HLint (hlint)


main :: IO ()
main = do
    hints <- hlint ["."]
    unless (null hints) exitFailure
