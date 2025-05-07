{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Censor where

import Control.Arrow
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

data C f i o a where
	Tell :: String -> a -> C f i o a
	C :: (String -> String) -> f i o a -> C f i o a

instance Union.HFunctor C
instance Union.HFunctorI C
instance Union.HFunctorO C

instance Union.HFunctor' C where
	hmap' _ g (Tell lg a) = Tell lg $ g a
	hmap' f _ (C k m) = C k $ f m

tell :: Union.Member C effs => String -> Eff.E effs i o ()
tell = Eff.effh . (`Tell` ())

censor :: Union.Member C effs =>
	(String -> String) -> Eff.E effs i o a -> Eff.E effs i o a
censor = (Eff.effh .) . C

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (C ': effs) i o a -> Eff.E effs i o (String, a)
run = \case
	HFreer.Pure x -> HFreer.Pure ("", x)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run ("" ,) u' HFreer.:>>= Q.singleton
			\(s, x) -> ((s ++) `first`) <$> run (q `HFreer.app` x)
		Right (Tell s a) -> ((s ++) `first`) <$> run (q `HFreer.app` a)
		Right (C f m) -> (f `first`) <$> run m
			>>= \(s, x) -> ((s ++) `first`) <$> run (q `HFreer.app` x)

run' :: Union.HFunctor (Union.U effs) =>
	(String -> String) -> Eff.E (C ': effs) i o a -> Eff.E effs i o (String, a)
run' f = \case
	HFreer.Pure x -> HFreer.Pure ("", x)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (run' f) ("" ,) u' HFreer.:>>= Q.singleton
			\(s, x) -> ((s ++) `first`) <$> run' f (q `HFreer.app` x)
		Right (Tell s a) -> ((f s ++) `first`) <$> run' f (q `HFreer.app` a)
		Right (C f' m) -> run' f' m
			>>= \(s, x) -> ((s ++) `first`) <$> run' f (q `HFreer.app` x)

censorHello, censorHello' :: Union.Member C effs => Eff.E effs i o ()
censorHello = censor (\case "Hello" -> "Goodbye"; s -> s) hello
censorHello' = censor (\case "Hello" -> "Goodbye"; s -> s) hello'

hello, hello' :: Union.Member C effs => Eff.E effs i o ()
hello = tell "Hello" >> tell " world!"
hello' = tell "Hello"
