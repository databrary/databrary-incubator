module Databrary.Service.Entropy
  ( Entropy
  , initEntropy
  , entropyBytes
  ) where

import Data.ByteArray (ByteArray)
import Crypto.Random.EntropyPool

type Entropy = EntropyPool

initEntropy :: IO Entropy
initEntropy = createEntropyPool

entropyBytes :: ByteArray a => Int -> Entropy -> IO a
entropyBytes = flip getEntropyFrom
