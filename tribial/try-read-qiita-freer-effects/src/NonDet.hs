{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NonDet where

import Control.Applicative
import Control.Monad
import Eff
import Freer
import OpenUnion

import NonDetable

makeChoiceA :: Alternative f => Eff (NonDet ': effs) a -> Eff effs (f a)
makeChoiceA = handleRelay (pure . pure) $ \m k -> case m of
	MZero -> pure empty
	MPlus -> liftM2 (<|>) (k True) (k False)

msplit :: Member NonDet effs => Eff effs a -> Eff effs (Maybe (a, Eff effs a))
msplit = go []
	where
	go jq (Pure x) = pure (Just (x, msum jq))
	go jq (u `Bind` q) = case prj u of
		Just MZero -> case jq of
			[] -> pure Nothing
			j : jq' -> go jq' j
		Just MPlus -> go (q False : jq) (q True)
		Nothing -> u `Bind` (go jq . q)
