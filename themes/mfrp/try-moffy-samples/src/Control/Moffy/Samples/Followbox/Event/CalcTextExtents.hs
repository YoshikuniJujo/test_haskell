{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.Event.CalcTextExtents (
	CalcTextExtents(..), pattern OccCalcTextExtents,
	TextExtents(..), Rectangle(..), FontName, FontSize, calcTextExtents
	) where

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
		deriving Show

data TextExtents = TextExtents {
	textExtentsInkRect :: Rectangle,
	textExtentsLogicalRect :: Rectangle } deriving Show

data Rectangle = Rectangle {
	rectangleLeft :: Double,
	rectangleTop :: Double,
	rectangleWidth :: Double,
	rectangleHeight :: Double } deriving Show

calcTextExtents :: FontName -> FontSize -> T.Text ->
	React s (Singleton CalcTextExtents) TextExtents
calcTextExtents fn fs t = maybe (calcTextExtents fn fs t) pure
	=<< await (CalcTextExtentsReq fn fs t)
		\(OccCalcTextExtents fn' fs' t' glp) ->
			bool Nothing (Just glp) $ (fn, fs, t) == (fn', fs', t')
