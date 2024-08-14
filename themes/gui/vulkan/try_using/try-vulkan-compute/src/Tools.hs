{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (tail', head') where

import Data.Maybe
import Data.List qualified as L

head' :: [a] -> a; head' = fst . fromJust . L.uncons
tail' :: [a] -> [a]; tail' = snd . fromJust . L.uncons
