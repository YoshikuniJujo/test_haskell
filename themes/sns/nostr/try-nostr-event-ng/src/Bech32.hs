{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bech32 where

import Control.Monad
import Data.Text qualified as T
import Codec.Binary.Bech32
import Data.ByteString qualified as BS
import Data.Text.Encoding

input :: T.Text
input = "example1f3hhyetdyp5hqum4d5sxgmmvdaezqumfwssxzmt9wsss9un3cx"

dataPart :: T.Text -> Maybe BS.ByteString
dataPart b = let Right (_, dataPartToBytes -> d) = decode b in d

dataPart' :: T.Text -> T.Text -> Either String BS.ByteString
dataPart' tg0 b = case decode b of
	Right (humanReadablePartToText -> tg, dataPartToBytes -> d) -> do
		if tg == tg0 then Right () else Left (T.unpack tg ++ "/=" ++ T.unpack tg0)
		maybe (Left "data is Nothing") Right d
	Left err -> Left $ show err ++ ": " ++ T.unpack b
