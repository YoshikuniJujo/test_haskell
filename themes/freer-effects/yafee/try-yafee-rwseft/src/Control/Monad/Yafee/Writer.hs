{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Writer (W(..), tell, run) where

import Control.Arrow
import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data W w a where Tell :: w -> W w ()

tell :: Union.Member (W w) effs => w -> Eff.E effs ()
tell = Eff.eff . Tell

run :: Monoid w => Eff.E (W w ': effs) a -> Eff.E effs (a, w)
run = Eff.handleRelay
	(pure . (, mempty)) \(Tell w) -> (((w <>) `second`) <$>) . ($ ())
