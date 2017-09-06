{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Control.Arrow
import Data.Monoid

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

data Writer w a where
	Writer :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Writer w

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = \case
	Pure x -> Pure (x, mempty)
	Join u q -> case decomp u of
		Right (Writer w) -> second (w <>) <$> runWriter (q `qApp` ())
		Left u' -> Join u' . tsingleton $ q `qComp` runWriter

data State s a where
	Get :: State s s
	Put :: !s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send Get

put :: Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = fmap f get >>= put

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
runState m s = case m of
	Pure x -> Pure (x, s)
	Join u q -> case decomp u of
		Right Get -> runState (q `qApp` s) s
		Right (Put s') -> runState (q `qApp` ()) s'
		Left u' -> Join u' . tsingleton $ q `qComp` (`runState` s)
