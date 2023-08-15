{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.List qualified as L
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Complex
import Text.Read
import Codec.Picture
import System.Environment

import TryMandelbrot

main :: IO ()
main = do
	as <- getArgs
	case as of
		[	(`parsePair` 'x') -> Just (w, h),
			(`parsePair` ',') -> Just (lft, upr),
			(`parsePair` ',') -> Just (rgt, lwr) ] ->
			writePng @Pixel8 "autogen/mandelbrot-cpu.png"
				. Image (fromIntegral w) (fromIntegral h)
				. VS.fromList . (fromIntegral <$>) . V.toList
				=<< render (w, h) (lft :+ upr) (rgt :+ lwr)
		_ -> error "bad command line arguments"

parsePair :: Read a => String -> Char -> Maybe (a, a)
parsePair s c = case L.findIndex (== c) s of
	Nothing -> Nothing
	Just i -> (,) <$> readMaybe (take i s) <*> readMaybe (tail $ drop i s)
