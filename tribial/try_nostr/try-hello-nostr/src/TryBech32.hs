{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryBech32 where

import Data.Text qualified as T
import Codec.Binary.Bech32
import Data.ByteString qualified as BS
import Data.Text.Encoding

input :: T.Text
input = "example1f3hhyetdyp5hqum4d5sxgmmvdaezqumfwssxzmt9wsss9un3cx"

dataPart :: T.Text -> Maybe BS.ByteString
dataPart b = let Right (_, dataPartToBytes -> d) = decode b in d
