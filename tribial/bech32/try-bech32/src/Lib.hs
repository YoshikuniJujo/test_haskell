{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Codec.Binary.Bech32
import Data.ByteString qualified as BS
import Data.Text qualified as T

someFunc :: T.Text -> BS.ByteString -> Either String T.Text
someFunc h d = do
	h' <- either (Left . show) pure $ humanReadablePartFromText h
	let	d' = dataPartFromBytes d
	either (Left . show) pure $ encode h' d'
