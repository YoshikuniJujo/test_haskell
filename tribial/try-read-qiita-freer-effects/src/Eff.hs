{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Eff (Eff, Freer(..), Member, run, runM, inj, prj, decomp) where

import Freer (Freer(..))
import OpenUnion (Union, Member, inj, prj, decomp, extract)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = pure x
runM (u `Bind` q) = runM . q =<< extract u
