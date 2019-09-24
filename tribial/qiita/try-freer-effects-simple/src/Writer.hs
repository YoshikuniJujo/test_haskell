{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Writer (Writer, tell, runWriter) where

import Control.Arrow (second)
import Data.Monoid (mempty, (<>))

import Eff (Eff, Freer(..), Member, inj, decomp)

data Writer w a where Writer :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell = (`Bind` Pure) . inj . Writer

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = \case
	Pure x -> Pure (x, mempty)
	u `Bind` k -> case decomp u of
		Right (Writer w) -> second (w <>) <$> runWriter (k ())
		Left u' -> u' `Bind` (runWriter . k)
