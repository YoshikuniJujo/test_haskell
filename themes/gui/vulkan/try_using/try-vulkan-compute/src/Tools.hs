{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (tail') where

import Data.Maybe
import Data.List qualified as L

tail' :: [a] -> [a]
tail' = snd . fromJust . L.uncons
