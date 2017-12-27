{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyEff.Internal (
	Eff, Member, run, runM, send, handleRelay, handleRelayS, interpose,
	Freer(..), NonDet(..), tsingleton, qApp, qComp, prj, decomp, extract
	) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Bool (bool)

import Private.Freer (Freer(..), tsingleton, qApp, qComp)
import Private.OpenUnion (Union, Member, inj, prj, decomp, extract)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "MyEff.run: expect Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = runM . (q `qApp`) =<< extract u

send :: Member eff effs => eff a -> Eff effs a
send = (`Join` tsingleton Pure) . inj

handleRelay ::
	(a -> Eff effs b) ->
	(forall x . eff x -> (x -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelay ret h = handleRelayS () (\() -> ret) (\() m k -> h m $ k ())

handleRelayS ::
	s -> (s -> a -> Eff effs b) ->
	(forall x . s -> eff x -> (s -> x -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelayS s0 ret h = loop s0
	where loop s = \case
		Pure x -> ret s x
		Join u q -> case decomp u of
			Right tx -> h s tx f
			Left u' -> Join u' . tsingleton $ f s
			where f = (q `qComp`) . loop

interpose :: Member eff effs =>
	(a -> Eff effs b) ->
	(forall x . eff x -> (x -> Eff effs b) -> Eff effs b) ->
	Eff effs a -> Eff effs b
interpose ret h = loop
	where loop = \case
		Pure x -> ret x
		Join u q -> case prj u of
			Just tx -> h tx f
			Nothing -> Join u $ tsingleton f
			where f = q `qComp` loop

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance Member NonDet effs => MonadPlus (Eff effs)

instance Member NonDet effs => Alternative (Eff effs) where
	empty = send MZero
	(<|>) = ((send MPlus >>=) .) . bool
