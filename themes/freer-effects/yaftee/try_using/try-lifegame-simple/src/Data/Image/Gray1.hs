{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Image.Gray1 (G(..), unconsRow) where

import Data.Vector qualified as V
import Data.Word
import Lifegame.Tools

data G = G { width :: Int, height :: Int, body :: V.Vector Word8 } deriving Show

unconsRow :: G -> Maybe (V.Vector Word8, G)
unconsRow G { height = h } | h < 1 = Nothing
unconsRow G { width = w, height = h, body = bd } = Just
	(V.take w' bd, G { width = w, height = h - 1, body = V.drop w' bd })
	where w' = w `div'` 8
