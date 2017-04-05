{-# LANGUAGE OverloadedStrings #-}

module Point (Point(..), toPoint) where

import Database.Esqueleto

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Point = Point Double Double deriving Show

instance PersistField Point where
	toPersistValue (Point lon lat) = PersistDbSpecific
		$ BS.concat ["(", ps $ lon, ", ", ps $ lat, ")"]
		where ps = BSC.pack . show

	fromPersistValue (PersistDbSpecific t) =
		Right . uncurry Point . read $ BSC.unpack t
	fromPersistValue _ =
		Left "Point values must be converted from PersistDbSpecific"

instance PersistFieldSql Point where
	sqlType _ = SqlOther "point"

toPoint :: Double -> Double -> Point
toPoint = Point
