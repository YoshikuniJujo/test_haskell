{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Mandelbrot.Draw (draw, Size, Pos) where

import Data.List qualified as L
import Data.Vector.Storable qualified as V
import Data.Complex
import Data.Word
import Codec.Picture
import Text.Read
import System.Environment
import Tools

draw :: FilePath ->
	(Size -> Complex Float -> Complex Float -> IO (V.Vector Word8)) -> IO ()
draw fp rndr = do
	[pair 'x' -> msz, pair ',' -> mlt, pair ',' -> mrb] <- getArgs
	case (msz, mlt, mrb) of
		(Just sz@(fromIntegral -> w, fromIntegral -> h),
				Just (l, t), Just (r, b)) ->
			writePng @Pixel8 fp . Image w h
				=<< rndr sz (l :+ t) (r :+ b)
		_ -> error "bad command line arguments"

type Size = (Word32, Word32)
type Pos = (Word32, Word32)

pair :: Read a => Char -> String -> Maybe (a, a)
pair c s = case L.span (/= c) s of
	(_, []) -> Nothing
	(t, d) -> (,) <$> (readMaybe t) <*> (readMaybe $ tail' d)
