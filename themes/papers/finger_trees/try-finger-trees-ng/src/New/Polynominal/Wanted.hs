{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Wanted where

import Outputable hiding (empty)

import Control.Arrow
import Data.Map.Strict

import New.Polynominal.AvoidNegative
import New.Polynominal.Zero hiding (containVars)
import New.Expression

import qualified New.Polynominal.Zero as Z

newtype Wanted v = Wanted (Zero v) deriving Show

expToWanted :: Ord v => Exp v Bool -> (Maybe (Wanted v), [Wanted v])
expToWanted = ((Wanted <$>) *** (Wanted <$>)) . \e -> eqToZero' e True empty

wantedToZero :: Wanted v -> Zero v
wantedToZero (Wanted z) = z

containVars :: Ord v => Wanted v -> [Maybe v]
containVars = Z.containVars . wantedToZero

selfContained :: Wanted v -> Bool
selfContained (Wanted z) = identity z

instance Show v => Outputable (Wanted v) where
	ppr = text . show

debugWanted = Wanted debugZeroWanted
