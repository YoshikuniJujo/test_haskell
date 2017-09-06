{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff where

import Freer
import OpenUnion

type Arr effs a b = a -> Eff effs b

type Arrs effs a b = FTCQueue (Eff effs) a b

type Eff effs = Freer (Union effs)

send :: Member eff effs => eff a -> Eff effs a
send t = Join (inj t) (tsingleton Pure)

qApp :: Arrs effs b w -> b -> Eff effs w
qApp q' x = case tviewl q' of
	TOne k -> k x
	k :| t -> case k x of
		Pure y -> qApp t y
		Join u q -> Join u (q >< t)

qComp :: Arrs effs a b -> (Eff effs b -> Eff effs' c) -> Arr effs' a c
qComp g h a = h $ g `qApp` a

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "bad"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = case extract u of
	mb -> mb >>= runM . (q `qApp`)
