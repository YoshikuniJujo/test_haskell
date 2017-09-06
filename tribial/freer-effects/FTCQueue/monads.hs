{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

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

data Reader e a where
	Reader :: Reader e e

ask :: Member (Reader e) effs => Eff effs e
ask = send Reader

runReader :: Eff (Reader e ': effs) a -> e -> Eff effs a
runReader m e = case m of
	Pure x -> Pure x
	Join u q -> case decomp u of
		Right Reader -> runReader (q `qApp` e) e
		Left u' -> Join u' . tsingleton $ q `qComp` (`runReader` e)
