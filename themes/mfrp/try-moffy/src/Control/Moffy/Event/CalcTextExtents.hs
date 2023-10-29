{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.CalcTextExtents (
	CalcTextExtents(..), pattern OccCalcTextExtents,
	TextExtents(..), Rectangle(..), FontName, FontSize, calcTextExtents
	) where

import Control.Moffy
import Control.Moffy.Event.Window
import Data.Type.Set
import Data.Bool

import qualified Data.Text as T

-- CALC TEXT EXTENTS

type FontName = String
type FontSize = Double

data CalcTextExtents = CalcTextExtentsReq WindowId FontName FontSize T.Text
	deriving (Show, Eq, Ord)
numbered [t| CalcTextExtents |]
instance Request CalcTextExtents where
	data Occurred CalcTextExtents =
		OccCalcTextExtents WindowId FontName FontSize T.Text TextExtents
		deriving Show

data TextExtents = TextExtents {
	textExtentsInkRect :: Rectangle,
	textExtentsLogicalRect :: Rectangle } deriving Show

data Rectangle = Rectangle {
	rectangleLeft :: Double,
	rectangleTop :: Double,
	rectangleWidth :: Double,
	rectangleHeight :: Double } deriving Show

calcTextExtents :: WindowId -> FontName -> FontSize -> T.Text ->
	React s (Singleton CalcTextExtents) TextExtents
calcTextExtents wid fn fs t = maybe (calcTextExtents wid fn fs t) pure
	=<< await (CalcTextExtentsReq wid fn fs t)
		\(OccCalcTextExtents wid' fn' fs' t' glp) ->
			bool Nothing (Just glp) $ (wid, fn, fs, t) == (wid', fn', fs', t')
