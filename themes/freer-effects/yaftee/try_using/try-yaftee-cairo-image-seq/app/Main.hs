{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.HigherOpenUnion qualified as U
import Data.CairoImage.Internal

import Data.Color

import Graphics.Pipe.Draw
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	img <- newImageArgb32Mut 16 16
	writeDrawPipe "foobar.png" img 16 16 case args of
		["blue"] -> examplePipe2
		_ -> examplePipe

examplePipe, examplePipe2 :: Argb32Mut RealWorld -> IO ()
examplePipe img =
	void $ Eff.runM . Pipe.run
		$ samplePipe @Double Pipe.=$= drawColor img [0 ..] (repeat [0 ..])

examplePipe2 img =
	void $ Eff.runM . Pipe.run
		$ samplePipe2 @Double Pipe.=$= drawColor img [0 ..] (repeat [0 ..])

samplePipe, samplePipe2 :: (
	RealFrac d,
	U.Member Pipe.P es
	) =>
	Eff.E es i [Rgba d] ()
samplePipe = (`mapM_` [0 .. 15]) \y -> Pipe.yield ((\x ->
	RgbaWord8 (x * 15 + 30) (y * 15 + 30) ((225 - x * 15 `div` 2 - y * 15 `div` 2) * 2 `div` 3) 255) <$> [0 .. 15])

samplePipe2 = (`mapM_` [0 .. 15]) \y -> Pipe.yield ((\x ->
	RgbaWord8
		((225 - x * 15 `div` 2 - y * 15 `div` 2) * 2 `div` 3)
		(y * 15 + 30)
		(x * 15 + 30)
		255) <$> [0 .. 15])
