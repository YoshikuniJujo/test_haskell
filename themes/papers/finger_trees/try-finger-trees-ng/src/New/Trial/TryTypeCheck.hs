{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module New.Trial.TryTypeCheck where

import New.Trial.ExpParser
import New.Polynominal.Wanted

wanted :: Wanted String
Just wanted = expToWanted . fst =<< parseBool (tokens "((p + d) == u)")
