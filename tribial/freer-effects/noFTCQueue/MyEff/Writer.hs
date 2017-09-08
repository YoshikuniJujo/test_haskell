{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.Writer where

import Control.Arrow
import Data.Monoid

import MyEff

data Writer w a where
	Writer :: w -> Writer w ()

tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Writer w

runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = \case
	Pure x -> Pure (x, mempty)
	Join u q -> case decomp u of
		Right (Writer w) -> second (w <>) <$> runWriter (q ())
		Left u' -> Join u' $ runWriter . q
