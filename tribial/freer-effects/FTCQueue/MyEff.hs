{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff (
	Eff, Freer(..), Member,
	run, runM, send, decomp, tsingleton, qApp, qComp) where

import Freer (Freer(..), tsingleton, qApp, qComp)
import OpenUnion (Union, Member, inj, decomp, extract)

type Eff effs = Freer (Union effs)

send :: Member eff effs => eff a -> Eff effs a
send t = Join (inj t) (tsingleton Pure)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "bad"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = case extract u of
	mb -> mb >>= runM . (q `qApp`)

-- あとですること
-- * decomp, tsingleton, qApp, qCompの代わりに、
-- 	もっと高レベルの関数を作って公開する
