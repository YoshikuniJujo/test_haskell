{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Cut (CutFalse, cutFalse) where

import MyEff (Eff, Member)
import MyEff.Exception (Exc, throwError)

data CutFalse = CutFalse

cutFalse :: Member (Exc CutFalse) r => Eff r a
cutFalse = throwError CutFalse
