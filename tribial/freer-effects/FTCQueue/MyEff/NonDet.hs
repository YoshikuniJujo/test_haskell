{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.NonDet (NonDet, fromList, makeChoiceA, msplit) where

import Control.Applicative (Alternative(..))
import Control.Monad (msum)

import MyEff.Internal (
	Eff, Member, handleRelay,
	Freer(..), NonDet(..), tsingleton, qApp, qComp, prj )

fromList :: Member NonDet effs => [a] -> Eff effs a
fromList = msum . map return

makeChoiceA :: Alternative t => Eff (NonDet ': effs) a -> Eff effs (t a)
makeChoiceA = handleRelay (return . pure) $ flip $ \f -> \case
	MZero -> return empty
	MPlus -> (<|>) <$> f False <*> f True

msplit :: Member NonDet effs => Eff effs a -> Eff effs (Maybe (a, Eff effs a))
msplit = loop []
	where loop jq = \case
		Pure x -> return $ Just (x, msum jq)
		Join u q -> case prj u of
			Just MZero -> case jq of
				[] -> return Nothing
				(j : jq') -> loop jq' j
			Just MPlus -> loop (q `qApp` True : jq) $ q `qApp` False
			Nothing -> Join u . tsingleton $ q `qComp` loop jq
