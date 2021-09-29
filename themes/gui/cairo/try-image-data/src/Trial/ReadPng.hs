{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.ReadPng where

import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture

readArgb32 :: FilePath -> IO (Maybe Argb32)
readArgb32 fp = readImage fp >>= \case
	Left em -> putStrLn em >> pure Nothing
	Right (ImageRGBA8 img) -> pure . Just $ juicyRGBA8ToCairoArgb32 img
	Right _ -> putStrLn "not RGBA8" >> pure Nothing
