{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.List.NonEmpty qualified as NE

import Dhall

data Example = Example { foo :: Natural, bar :: Vector Double }
	deriving (Generic, Show)

instance FromDhall Example

data Foo = Foo Natural | Bar String
	deriving (Generic, Show)

instance FromDhall Foo

data OneTwoThree = One | Two | Three deriving (Generic, Show)

instance FromDhall OneTwoThree

main :: IO ()
main = do
	x <- input auto "./config"
	y <- input auto "./foo"
	z <- input auto "./dummy"
	w <- input auto "./poly"
	a <- input auto "./nonempty"
	b <- input auto "./onetwothree"
	print (x :: Example)
	print (y :: Foo)
	print (z :: Dummy)
	print (w :: FishParam)
	print (a :: NonEmpty Double)
	print (b :: OneTwoThree)

data FishParam = FishParam {
	oneSide :: OneSide,
	pattern :: [Poly],
	colors :: Colors }
	deriving (Generic, Show)

instance FromDhall FishParam

data Colors = Colors {
	color1 :: Color, color2 :: Color, color3 :: Color }
	deriving (Generic, Show)

instance FromDhall Colors

data Color = Brown | Red | White deriving (Generic, Show)

instance FromDhall Color

data OneSide = OneSide (NonEmpty (Double, Double))
	deriving (Generic, Show)

instance FromDhall OneSide

data Poly
	= Polyline (NonEmpty (Double, Double))
	| Polygon (NonEmpty (Double, Double))
	deriving (Generic, Show)

newtype NonEmpty a = NonEmpty (NE.NonEmpty a)
	deriving Show deriving newtype Generic

instance FromDhall a => FromDhall (NonEmpty a)

instance FromDhall Poly

data Dummy = Dummy (Double, Double) deriving (Generic, Show)

instance FromDhall Dummy
