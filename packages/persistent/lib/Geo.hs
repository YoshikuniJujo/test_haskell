{-# LANGUAGE OverloadedStrings #-}

module Geo (Geo, toPoint) where

import Database.Esqueleto

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data Geo = Geo BS.ByteString deriving Show

instance PersistField Geo where
	toPersistValue (Geo t) = PersistDbSpecific t

	fromPersistValue (PersistDbSpecific t) =
		Right . Geo $ BS.concat ["'", t, "'"]
	fromPersistValue _ =
		Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
	sqlType _ = SqlOther "point"

toPoint :: Double -> Double -> Geo
toPoint lat lon = Geo $ BS.concat ["(", ps $ lon, ", ", ps $ lat, ")"]
	where ps = BSC.pack . show
