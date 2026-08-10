{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Text qualified as T
import Codec.Binary.Bech32
import Codec.Binary.Bech32.Internal

main :: IO ()
main = do
	print $ decode example
	Right (hrp, dp) <- pure $ decodeLenient example
	print hrp
	print dp
	print $ dataPartIsValid dp
	print $ dataPartToBytes dp

example :: T.Text
example = "ncryptsec1" <>
	"qgg9947rlpvqu76pj5ecreduf9jxhselq2nae2kghhvd5g7dgjtc" <>
	"xfqtd67p9m0w57lspw8gsq6yphnm8623nsl8xn9j4jdzz84zm3fr" <>
	"ztj3z7s35vpzmqf6ksu8r89qk5z2zxfmu5gv8th8wclt0h4p"
