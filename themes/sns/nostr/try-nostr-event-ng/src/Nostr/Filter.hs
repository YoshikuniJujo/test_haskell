{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Nostr.Filter where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.UnixTime
import Crypto.Curve.Secp256k1

data Filter = Filter {
	ids :: Maybe [BS.ByteString],
	authors :: Maybe [Pub],
	kinds :: Maybe [Int],
	tags :: [(Char, [T.Text])],
	since :: Maybe UnixTime, until :: Maybe UnixTime,
	limit :: Maybe Int }
	deriving Show
