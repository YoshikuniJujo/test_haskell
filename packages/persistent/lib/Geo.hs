{-# LANGUAGE OverloadedStrings #-}

module Geo (Geo(..), toPoint) where

import Database.Esqueleto

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Geo = Geo Double Double deriving Show

instance PersistField Geo where
	toPersistValue (Geo lon lat) = PersistDbSpecific
		$ BS.concat ["(", ps $ lon, ", ", ps $ lat, ")"]
		where ps = BSC.pack . show

	fromPersistValue (PersistDbSpecific t) =
		Right . uncurry Geo . read $ BSC.unpack t
	fromPersistValue _ =
		Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
	sqlType _ = SqlOther "point"

toPoint :: Double -> Double -> Geo
toPoint = Geo
