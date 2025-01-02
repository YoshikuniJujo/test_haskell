{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Pixels
import Draw

margin :: Margin
margin = Margin {
	topMargin = 20, bottomMargin = 20, leftMargin = 20, rightMargin = 20 }

unit :: CDouble
unit = 30

number :: Int
number = 12

radius :: CDouble
radius = 5

radius' :: CDouble
radius' = 3

main :: IO ()
main = withPng' "nearest.png" margin unit number \cr -> do
	forXyv_ distXs distYs nearestColors \x y c -> do
		rectangle cr margin unit x y
		fill cr c
	grid cr margin unit number
	forXyv_ distXs distYs nearestColors \x y c -> do
		circle cr margin unit x y radius'
		strokeAndFill cr 2 gray c
	forXyv_ srcXs srcYs colors \x y c -> do
		circle cr margin unit x y radius
		strokeAndFill cr 2 gray c
