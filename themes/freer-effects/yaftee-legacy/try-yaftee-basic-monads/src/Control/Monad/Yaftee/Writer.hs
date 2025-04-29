{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Writer (

	-- * NORMAL

	W, tell, run,

	-- * NAMED

	Named, tellN, runN

	) where

import GHC.TypeLits
import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor

-- * NORMAL

{-
type W w = Union.FromFirst (W_ w)

data W_ w a where Tell :: w -> W_ w ()

tell :: Union.Member (W w) effs => w -> Eff.E effs ()
tell = Eff.eff . Tell

run :: (HFunctor.H (Union.U effs), Monoid w) =>
	Eff.E (W w ': effs) a -> Eff.E effs (a, w)
run = (uncurry (flip (,)) <$>)
	. Eff.handleRelay (mempty ,) snd \(Tell w) k -> first (w <>) <$> k ()
	-}

type W w = Named "" w

tell :: Union.Member (W w) effs => w -> Eff.E effs ()
tell = tellN ""

run :: (HFunctor.H (Union.U effs), Monoid w) =>
	Eff.E (W w ': effs) a -> Eff.E effs (a, w)
run = runN

-- * NAMED

type Named nm w = Union.FromFirst (Named_ nm w)
data Named_ (nm :: Symbol) w a where TellN :: forall nm w . w -> Named_ nm w ()

tellN :: forall nm -> Union.Member (Named nm w) effs => w -> Eff.E effs ()
tellN nm = Eff.eff . TellN @nm

runN :: (HFunctor.H (Union.U effs), Monoid w) =>
	Eff.E (Named nm w ': effs) a -> Eff.E effs (a, w)
runN = (uncurry (flip (,)) <$>)
	. Eff.handleRelay (mempty ,) snd \(TellN w) k -> first (w <>) <$> k ()
