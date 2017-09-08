{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Writer.Internal (Writer(..), runWriter, tell) where

import Control.Arrow (second)
import Data.Monoid (mempty, (<>))

import MyEff (Eff, Member, send, handleRelay)

data Writer w a where Writer :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell = send . Writer

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = handleRelay (pure . (, mempty))
	$ \(Writer w) f -> second (w <>) <$> f ()
