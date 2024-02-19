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

main :: IO ()
main = do
	x <- input auto "./config"
	y <- input auto "./foo"
	z <- input auto "./dummy"
	w <- input auto "./poly"
	a <- input auto "./nonempty"
	print (x :: Example)
	print (y :: Foo)
	print (z :: Dummy)
	print (w :: [Poly])
	print (a :: NonEmpty Double)

data Poly
	= Polyline (NonEmpty (Double, Double))
	| Polygon (NonEmpty (Double, Double))
	deriving (Generic, Show)

newtype NonEmpty a = NonEmpty (NE.NonEmpty a)
	deriving Show deriving newtype Generic

-- instance FromDhall a => FromDhall (NE.NonEmpty a)

instance FromDhall a => FromDhall (NonEmpty a)

instance FromDhall Poly

data Dummy = Dummy (Double, Double) deriving (Generic, Show)

instance FromDhall Dummy
