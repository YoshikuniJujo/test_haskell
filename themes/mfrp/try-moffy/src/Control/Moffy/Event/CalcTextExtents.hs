{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.CalcTextExtents where

import Control.Moffy
import Data.Type.Set
import Data.Bool

import qualified Data.Text as T

-- CALC TEXT EXTENTS

type FontName = String
type FontSize = Double

data CalcTextExtents = CalcTextExtentsReq FontName FontSize T.Text
	deriving (Show, Eq, Ord)
numbered [t| CalcTextExtents |]
instance Request CalcTextExtents where
	data Occurred CalcTextExtents =
		OccCalcTextExtents FontName FontSize T.Text TextExtents

data TextExtents = TextExtents {
	textExtentsXBearing :: Double,
	textExtentsYBearing :: Double,
	textExtentsWidth :: Double,
	textExtentsHeight :: Double,
	textExtentsXAdvance :: Double,
	textExtentsYAdvance :: Double } deriving Show

calcTextExtents :: FontName -> FontSize -> T.Text ->
	React s (Singleton CalcTextExtents) TextExtents
calcTextExtents fn fs t = maybe (calcTextExtents fn fs t) pure
	=<< await (CalcTextExtentsReq fn fs t)
		\(OccCalcTextExtents fn' fs' t' glp) ->
			bool Nothing (Just glp) $ (fn, fs, t) == (fn', fs', t')
