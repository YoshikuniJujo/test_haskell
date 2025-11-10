{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.State (

-- * NORMAL

S, get, gets, put, modify, modify', getsModify, run,

-- * NAMED

Named, getN, getsN, putN, modifyN, modifyN', getsModifyN, runN

) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.HigherFunctor qualified as HFunctor

-- * NORMAL

type S s = Named "" s

get :: Union.Member (S s) effs => Eff.E effs i o s
get = getN ""

gets :: Union.Member (S s) effs => (s -> a) -> Eff.E effs i o a
gets = getsN ""

put :: Union.Member (S s) effs => s -> Eff.E effs i o ()
put = putN ""

modify, modify' :: Union.Member (S s) effs => (s -> s) -> Eff.E effs i o ()
modify = modifyN ""
modify' = modifyN' ""

getsModify :: Union.Member (S s) effs =>
	(s -> Maybe (a, s)) -> Eff.E effs i o (Maybe a)
getsModify = getsModifyN ""

run :: HFunctor.Loose (Union.U effs) =>
	Eff.E (S s ': effs) i o a -> s -> Eff.E effs i o (a, s)
run = runN

-- * NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()
	Modify :: forall nm s . (s -> s) -> Named_ nm s ()
	GetsModify :: forall nm s a . (s -> Maybe (a, s)) -> Named_ nm s (Maybe a)

getN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs i o s
getN nm = Eff.eff (Get @nm)

getsN :: forall s effs i o a . forall nm ->
	Union.Member (Named nm s) effs => (s -> a) -> Eff.E effs i o a
getsN nm f = f <$> getN nm

putN :: forall s effs i o . forall nm ->
	Union.Member (Named nm s) effs => s -> Eff.E effs i o ()
putN nm = Eff.eff . Put @nm

{-
modifyN :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs i o ()
modifyN nm = Eff.eff . Modify @nm
-}

modifyN :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs i o ()
modifyN nm f = void $ getsModifyN nm (Just . (() ,) . f)

modifyN' :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs i o ()
modifyN' nm f = putN nm . f =<< getN nm

getsModifyN :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> Maybe (a, s)) -> Eff.E effs i o (Maybe a)
getsModifyN nm = Eff.eff . GetsModify @nm

runN :: forall nm effs s i o a .
	HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (a, s)
runN = ((uncurry (flip (,)) <$>) .) .  Eff.handleRelayS (,) fst snd \st k s ->
	case st of
		Get -> k s $! s; Put s' -> k () s'; Modify f -> k () $! (f $! s)
		GetsModify f -> case f s of
			Nothing -> k Nothing s
			Just (x, s') -> k (Just x) s'
