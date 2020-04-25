{-# OPTIONS_GHC -fno-warn-orphans #-}
module DeepseqInstance () where

import Control.DeepSeq (NFData, NFData1)

import Data.ShiftMap.Internal (Balance, ShiftMap)

instance NFData Balance

instance NFData a => NFData (ShiftMap a)
instance NFData1 ShiftMap
