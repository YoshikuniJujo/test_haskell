module MArrayTools where

import Data.Array.MArray

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = writeArray a i . f =<< readArray a i
