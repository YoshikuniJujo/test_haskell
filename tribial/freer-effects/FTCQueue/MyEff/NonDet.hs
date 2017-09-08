{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MyEff.NonDet where

import Control.Applicative
import Control.Monad

import MyEff.Internal

data NonDet a where
	MZero :: NonDet a
	MPlus :: NonDet Bool

instance Member NonDet effs => Alternative (Eff effs) where
	empty = mzero
	(<|>) = mplus

instance Member NonDet effs => MonadPlus (Eff effs) where
	mzero = send MZero
	mplus m1 m2 = send MPlus >>= \x -> if x then m1 else m2

makeChoiceA :: Alternative f => Eff (NonDet ': effs) a -> Eff effs (f a)
makeChoiceA = handleRelay (return . pure) $ \m k -> case m of
	MZero -> return empty
	MPlus -> liftM2 (<|>) (k True) (k False)

msplit :: Member NonDet effs => Eff effs a -> Eff effs (Maybe (a, Eff effs a))
msplit = loop []
	where loop jq = \case
		Pure x -> return $ Just (x, msum jq)
		Join u q -> case prj u of
			Just MZero -> case jq of
				[] -> return Nothing
				(j : jq') -> loop jq' j
			Just MPlus -> loop (q `qApp` False : jq) (q `qApp` True)
			Nothing -> Join u $ tsingleton k
			where k = q `qComp` loop jq
