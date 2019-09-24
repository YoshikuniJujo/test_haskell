{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Eff (Eff, Member, run, send, Freer(..), prj, decomp) where

import Freer (Freer(..))
import OpenUnion (Union, Member, inj, prj, decomp)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

send :: Member eff effs => eff a -> Eff effs a
send = (`Bind` Pure) . inj
