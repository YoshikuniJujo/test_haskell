{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Cut where

import MyEff
import MyEff.Exception

data CutFalse = CutFalse

cutFalse :: Member (Exc CutFalse) r => Eff r a
cutFalse = throwError CutFalse
