{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

import Lib

main :: IO ()
main = do
	fp : _ <- getArgs
	img <- readImageRGB8 fp
	print . BS.length . BS.pack . V.toList $ imageData img
	BS.writeFile "../texture0/tire.raw" . BS.pack . V.toList $ imageData img
	img' <- readImageRGBA8 fp
	print . BS.length . BS.pack . V.toList $ imageData img'
	BS.writeFile "../texture0/tire2.raw" . BS.pack . V.toList $ imageData img'
