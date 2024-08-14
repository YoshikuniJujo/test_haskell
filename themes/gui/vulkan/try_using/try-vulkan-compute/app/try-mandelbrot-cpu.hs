{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS

import TryMandelbrot
import Mandelbrot.Draw

main :: IO ()
main = draw "autogen/mandelbrot-cpu.png" \sz lt rb ->
	VS.fromList . V.toList . (fromIntegral <$>) <$> render sz lt rb
