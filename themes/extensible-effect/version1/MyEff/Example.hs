{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Example where

import Data.Typeable

import MyEff
import MyEff.Lift
import MyEff.Writer
import TypeLevel

writeAll :: (Typeable a, Member (Writer a) e) => [a] -> Eff e ()
writeAll = mapM_ tell
