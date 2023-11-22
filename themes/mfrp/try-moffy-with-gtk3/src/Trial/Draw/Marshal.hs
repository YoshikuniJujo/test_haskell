{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw.Marshal (Shape(..), Point, Color(..), readFile, writeFile) where

import Prelude hiding (readFile, writeFile)

import GHC.Generics
import Codec.Serialise

import qualified Data.ByteString.Lazy as BSL

type Point = (Double, Double)
data Color = Color {
	colorRed :: Double,
	colorGreen :: Double,
	colorBlue :: Double } deriving (Show, Generic)

data Shape = Line Point Point | FillPolygon Color [Point] deriving (Show, Generic)

instance Serialise Shape
instance Serialise Color

magic :: BSL.ByteString
magic = "\xDE\xAD\xBE\xEF\x63\x26\x44\x92"

writeFile :: FilePath -> [Shape] -> IO ()
writeFile fp = BSL.writeFile fp . (magic <>) . serialise

checkMagic :: BSL.ByteString -> Either String BSL.ByteString
checkMagic (BSL.splitAt 8 -> (m, bs))
	| m == magic = Right bs
	| otherwise = Left "bad magic"

readFile :: FilePath -> IO (Either String [Shape])
readFile fp = do
	bs <- BSL.readFile fp
	pure $ do
		ss <- checkMagic bs
		either (Left . show) Right $ deserialiseOrFail ss
