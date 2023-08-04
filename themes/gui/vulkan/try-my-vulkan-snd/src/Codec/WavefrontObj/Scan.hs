{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.WavefrontObj.Scan (

	-- * WAVEFRONT OBJ SCAN

	s, s_, Atom(..), Vertex(..)

	) where

import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

removeComment :: BS.ByteString -> [BS.ByteString]
removeComment = filter (not . ("#" `BS.isPrefixOf`)) . BSC.lines

nonEmptyWords :: [BS.ByteString] -> [NonEmpty BS.ByteString]
nonEmptyWords = catMaybes . (NE.nonEmpty . BSC.words <$>)

data Atom i
	= V Float Float Float
	| Vt Float Float
	| Vn Float Float Float
	| F (Vertex i) (Vertex i) (NonEmpty (Vertex i))
	| L (Vertex i) (NonEmpty (Vertex i))
	| Mtllib FilePath
	| Usemtl String
	| O String
	| G String
	| S Bool
	| Unimplemented String
	deriving Show

data Vertex i = Vertex {
	vertexPosition :: i,
	vertexTexture :: Maybe i,
	vertexNormal :: Maybe i }
	deriving Show

waveFrontAtom :: Read i => NonEmpty BS.ByteString -> Atom i
waveFrontAtom = \case
	"v" :| [x, y, z] -> V (bread x) (bread y) (bread z)
	"vt" :| [x, y] -> Vt (bread x) (bread y)
	"vn" :| [x, y, z] -> Vn (bread x) (bread y) (bread z)
	"f" :| (a : b : c : vs) -> F (vertex a) (vertex b) (vertex c :| (vertex <$> vs))
	"l" :| (a : b : vs) -> L (vertex a) (vertex b :| (vertex <$> vs))
	"mtllib" :| [fp] -> Mtllib $ BSC.unpack fp
	"usemtl" :| [nm] -> Usemtl $ BSC.unpack nm
	"o" :| [nm] -> O $ BSC.unpack nm
	"g" :| [nm] -> G $ BSC.unpack nm
	"s" :| ["1"] -> S True
	"s" :| ["off"] -> S False
	ws -> Unimplemented . BSC.unpack $ BSC.unwords $ NE.toList ws

vertex :: Read i => BS.ByteString -> Vertex i
vertex cs = case BSC.split '/' cs of
	[bread -> p] -> Vertex p Nothing Nothing
	[bread -> p, bread -> t] -> Vertex p (Just t) Nothing
	[bread -> p, t, bread -> n]
		| BS.null t -> Vertex p Nothing (Just n)
		| otherwise -> Vertex p (Just $ bread t) (Just n)
	_ -> error "bad vertex"

bread :: Read a => BS.ByteString -> a
bread = read . BSC.unpack

s :: (Monad m, Read i) => (Atom i -> m a) -> BS.ByteString -> m [a]
s f = (f `mapM`) . (waveFrontAtom <$>) . nonEmptyWords . removeComment

s_ :: (Monad m, Read i) => (Atom i -> m a) -> BS.ByteString -> m ()
s_ f = (f `mapM_`) . (waveFrontAtom <$>) . nonEmptyWords . removeComment
