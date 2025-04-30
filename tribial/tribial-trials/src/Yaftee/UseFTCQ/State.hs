{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.State where

import GHC.TypeLits

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

-- * NORMAL

type S s = Named "" s

get :: Union.Member (S s) effs => Eff.E effs i o s
get = getN ""

gets :: Union.Member (S s) effs => (s -> t) -> Eff.E effs i o t
gets = getsN ""

put :: Union.Member (S s) effs => s -> Eff.E effs i o ()
put = putN ""

modify :: Union.Member (S s) effs => (s -> s) -> Eff.E effs i o ()
modify = modifyN ""

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (S s ': effs) i o a -> s -> Eff.E effs i o (a, s)
run = runN

-- * NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	GetN :: Named_ nm s s; PutN :: forall nm s . !s -> Named_ nm s ()

getN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs i o s
getN nm = Eff.eff (GetN @nm)

getsN :: forall s t effs i o .
	forall nm -> Union.Member (Named nm s) effs => (s -> t) -> Eff.E effs i o t
getsN nm f = f <$> getN nm

putN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => s -> Eff.E effs i o ()
putN nm = Eff.eff . PutN @nm

modifyN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => (s -> s) -> Eff.E effs i o ()
modifyN nm f = putN nm . f =<< getN nm

runN :: Union.HFunctor (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (a, s)
runN = (((\(x, y) -> (y, x)) <$>) .) . Eff.handleRelayS (flip (,)) snd fst \st k s ->
	case st of GetN -> k s s; PutN s' -> k () s'

-- * SAMPLE

sample :: (
	Union.Member (S Int) effs,
	Union.Member (Union.FromFirst IO) effs ) => Eff.E effs i o ()
sample = do
	put (123 :: Int)
	Eff.eff . print =<< get @Int
