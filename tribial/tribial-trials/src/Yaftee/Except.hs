{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Except where

import Control.Monad
import Data.Kind

import Yaftee.Eff qualified as Eff
import Yaftee.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.TypeElem qualified as Elem

data E e (f :: Type -> Type -> Type -> Type) i o a where
	Throw :: e -> E e f i o a
	Catch :: f i o a -> (e -> f i o a) -> E e f i o a

throw :: Elem.Member (E e) effs => e -> Eff.E effs i o a
throw = Eff.effh . Throw

m `catch` h = Eff.effh $ Catch m h

run :: Union.HFunctor (Union.U effs) => Eff.E (E e ': effs) i o a -> Eff.E effs i o (Either e a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmap run Right u' HFreer.:>>= either (HFreer.Pure . Left) (run . k)
		Right (Throw e) -> HFreer.Pure $ Left e
		Right (m `Catch` h) -> either (HFreer.Pure . Left) (run . k)
			=<< either (run . h) (HFreer.Pure . Right) =<< run m

sample :: (
	Elem.Member (E String) effs,
	Elem.Member (Union.FromFirst IO) effs ) =>
	Int -> Int -> Eff.E effs i o ()
sample m n = do
	when (n == 0) $ throw "zero division"
	Eff.eff . print $ m `div` n

sample' m n = sample m n `catch` \e -> Eff.eff . putStrLn $ "cathed! message is `" ++ e ++ "'"

instance Union.HFunctor (E e) where
	hmap _ _ (Throw e) = Throw e
	hmap f _ (m `Catch` h) = f m `Catch` \e -> f $ h e
