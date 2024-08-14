{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Vector.Storable qualified as V
import Mandelbrot.Draw
import TryMandelbrot

main :: IO ()
main = draw "autogen/mandelbrot-cpu.png" \sz lt rb ->
	V.fromList . (fromIntegral <$>) <$> render sz lt rb
