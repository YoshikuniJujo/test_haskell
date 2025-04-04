{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Writer where

import Control.Arrow
import Control.Monad.Yafe.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data Writer w a where Tell :: w -> Writer w ()

tell :: Union.Member (Writer w) effs => w -> Eff.E effs ()
tell = Eff.eff . Tell

runWriter :: Monoid w => Eff.E (Writer w ': effs) a -> Eff.E effs (a, w)
runWriter = Eff.handleRelay
	(pure . (, mempty)) \(Tell w) -> (((w <>) `second`) <$>) . ($ ())
