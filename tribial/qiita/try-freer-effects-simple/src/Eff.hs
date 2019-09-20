{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Eff (Eff, Member, run, runM, send, Freer(..), decomp) where

import Freer (Freer(..))
import OpenUnion (Union, Member, inj, decomp, extract)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "MyEff.run: This function can run only Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Bind u q) = runM . q =<< extract u

send :: Member eff effs => eff a -> Eff effs a
send = (`Bind` Pure) . inj
