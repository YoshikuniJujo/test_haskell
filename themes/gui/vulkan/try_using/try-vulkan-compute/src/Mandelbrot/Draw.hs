{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Mandelbrot.Draw (draw) where

import Data.List qualified as L
import Data.Vector.Storable qualified as V
import Data.Word
import Data.Complex
import Codec.Picture
import Text.Read
import System.Environment

import Tools

draw :: FilePath ->
	(Size -> Complex Float -> Complex Float -> IO (V.Vector Word8)) -> IO ()
draw fp rndr = do
	[sz_, lt_, rb_] <- getArgs
	let	msz = parsePair sz_ 'x'
		mlt = parsePair lt_ ','
		mrb = parsePair rb_ ','
	case (msz, mlt, mrb) of
		(Just sz@(fromIntegral -> w, fromIntegral -> h), Just (lft, upr), Just (rgt, lwr)) ->
			writePng @Pixel8 fp
				. Image w h =<< rndr sz (lft :+ upr) (rgt :+ lwr)
		_ -> error "bad command line arguments"

type Size = (Word32, Word32)

parsePair :: Read a => String -> Char -> Maybe (a, a)
parsePair s c = case L.findIndex (== c) s of
	Nothing -> Nothing
	Just i -> (,) <$> (readMaybe $ take i s) <*> (readMaybe . tail' $ drop i s)
