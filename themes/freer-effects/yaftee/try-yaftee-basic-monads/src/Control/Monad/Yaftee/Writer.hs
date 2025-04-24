{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Writer where

import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor

type W w = Union.FromFirst (W_ w)

data W_ w a where Tell :: w -> W_ w ()

tell :: Union.Member (W w) effs => w -> Eff.E effs ()
tell = Eff.eff . Tell

run :: (HFunctor.H (Union.U effs), Monoid w) =>
	Eff.E (W w ': effs) a -> Eff.E effs (a, w)
run = (uncurry (flip (,)) <$>)
	. Eff.handleRelay (mempty ,) snd \(Tell w) k -> first (w <>) <$> k ()
