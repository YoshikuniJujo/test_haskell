module Data.Bits.Utils where

import Data.Bits

checkBits :: Bits bs => bs -> bs -> Bool
checkBits bs0 = (== bs0) . (.&. bs0)
