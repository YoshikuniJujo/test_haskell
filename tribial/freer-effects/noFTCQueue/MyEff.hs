{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff (
	Eff, Member, run, runM, send,
	Freer(..), decomp
	) where

import Freer
import OpenUnion

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "Internal:run This (E) should never happen"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = case extract u of
	mb -> mb >>= runM . q

send :: Member eff effs => eff a -> Eff effs a
send t = Join (inj t) Pure
