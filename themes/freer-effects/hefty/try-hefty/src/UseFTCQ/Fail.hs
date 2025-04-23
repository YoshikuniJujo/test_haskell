{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.Fail where

import UseFTCQ.Eff qualified as Eff
import UseFTCQ.HFreer qualified as HFreer
import Data.FTCQueue qualified as Q
import OpenUnion qualified as Union

type F = Union.FromFirst Union.Fail

{-# COMPLETE F #-}

pattern F :: String -> F f a
pattern F msg = Union.FromFirst (Union.Fail msg)

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (F ': effs) a -> Eff.E effs (Either String a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run Right u' HFreer.:>>=
			Q.singleton (either (HFreer.Pure . Left) (run `HFreer.comp` q))
		Right (F msg) -> HFreer.Pure $ Left msg
