{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Pipe qualified as Pipe

import Data.ByteString qualified as BS
import Data.CairoImage.Internal

import Graphics.Pipe.Draw
import Graphics.Pipe.Write

main :: IO ()
main = do
	img <- newImageMut @Argb32Mut 16 16
	writeDrawPipe [] "try-yaftee-cairo-image-exe.png" 16 16 img (void . id)
		$ Pipe.yield (BS.concat (replicate 195 "\0\0\0\255")) Pipe.=$=
			drawCairoImageRgba32 IO img 16 16
