{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Maybe where

import Eff (Eff, Freer(..), Member, inj, decomp)

data MaybeE a = NothingE deriving Show

nothing :: Member MaybeE effs => Eff effs a
nothing = inj NothingE `Bind` Pure

runMaybe :: Eff (MaybeE ': effs) a -> Eff effs (Maybe a)
runMaybe = \case
	Pure x -> Pure $ Just x
	u `Bind` k -> case decomp u of
		Right NothingE -> Pure Nothing
		Left u' -> u' `Bind` (runMaybe . k)
