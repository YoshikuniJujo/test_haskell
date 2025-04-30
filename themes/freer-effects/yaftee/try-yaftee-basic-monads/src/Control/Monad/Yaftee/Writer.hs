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
import Data.HigherFunctor qualified as HFunctor

-- * NORMAL

type W w = Named "" w

tell :: Union.Member (W w) effs => w -> Eff.E effs i o ()
tell = tellN ""

run :: (HFunctor.Loose (Union.U effs), Monoid w) =>
	Eff.E (W w ': effs) i o a -> Eff.E effs i o (a, w)
run = runN

-- * NAMED

type Named nm w = Union.FromFirst (Named_ nm w)
data Named_ (nm :: Symbol) w a where TellN :: forall nm w . w -> Named_ nm w ()

tellN :: forall nm -> Union.Member (Named nm w) effs => w -> Eff.E effs i o ()
tellN nm = Eff.eff . TellN @nm

runN :: (HFunctor.Loose (Union.U effs), Monoid w) =>
	Eff.E (Named nm w ': effs) i o a -> Eff.E effs i o (a, w)
runN = (uncurry (flip (,)) <$>)
	. Eff.handleRelay (mempty ,) snd \(TellN w) k -> ((w <>) `first`) <$> k ()
