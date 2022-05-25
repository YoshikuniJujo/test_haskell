{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Codec.Wavefront
import Codec.Wavefront.IO

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

getSample :: IO WavefrontOBJ
getSample = either error pure =<< fromFile "../../files/models/viking_room.obj"

getTiny :: IO WavefrontOBJ
getTiny = either error pure =<< fromFile "tiny.obj"

readTiny :: IO BS.ByteString
readTiny = BS.readFile "tiny.obj"

removeComment :: BS.ByteString -> [BS.ByteString]
removeComment = filter (not . ("#" `BS.isPrefixOf`)) . BSC.lines

nonEmptyWords :: [BS.ByteString] -> [NonEmpty BS.ByteString]
nonEmptyWords = catMaybes . (NE.nonEmpty . BSC.words <$>)

data WavefrontAtom
	= V Double Double Double
	| Vt Double Double
	| Vn Double Double Double
	| F Vertex Vertex Vertex
	| Mtllib FilePath
	| Usemtl String
	| O String
	| G String
	| S Bool
	| Unimplemented String
	deriving Show

isUnimplemented :: WavefrontAtom -> Bool
isUnimplemented = \case Unimplemented _ -> True; _ -> False

data Vertex = Vertex {
	vertexPosition :: Int,
	vertexTexture :: Maybe Int,
	vertexNormal :: Maybe Int }
	deriving Show

waveFrontAtom :: NonEmpty BS.ByteString -> WavefrontAtom
waveFrontAtom = \case
	"v" :| [x, y, z] -> V (bread x) (bread y) (bread z)
	"vt" :| [x, y] -> Vt (bread x) (bread y)
	"vn" :| [x, y, z] -> V (bread x) (bread y) (bread z)
	"f" :| [a, b, c] -> F (vertex a) (vertex b) (vertex c)
	"mtllib" :| [fp] -> Mtllib $ BSC.unpack fp
	"usemtl" :| [nm] -> Usemtl $ BSC.unpack nm
	"o" :| [nm] -> O $ BSC.unpack nm
	"g" :| [nm] -> G $ BSC.unpack nm
	"s" :| ["1"] -> S True
	"s" :| ["off"] -> S False
	s -> Unimplemented . BSC.unpack $ BSC.unwords $ NE.toList s

vertex :: BS.ByteString -> Vertex
vertex s = case BSC.split '/' s of
	[bread -> p] -> Vertex p Nothing Nothing
	[bread -> p, bread -> t] -> Vertex p (Just t) Nothing
	[bread -> p, t, bread -> n]
		| BS.null t -> Vertex p Nothing (Just n)
		| otherwise -> Vertex p (Just $ bread t) (Just n)
	_ -> error "bad vertex"

bread :: Read a => BS.ByteString -> a
bread = read . BSC.unpack
